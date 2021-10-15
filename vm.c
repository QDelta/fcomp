#include <stddef.h>
#include <stdio.h>
#include <string.h>

extern void exit(int);

typedef unsigned char byte_t;
typedef long long int_t;
typedef void *ptr_t;
typedef void (*func_t)(void);

#define OFFSET(p, o) ((ptr_t)(p) + (o))

/** stack, heap **/

#ifndef MEM_SIZE
#define MEM_SIZE (1ULL << 24)
#endif
static byte_t memory[MEM_SIZE]; 

static ptr_t bp = memory + MEM_SIZE;
static ptr_t sp = memory + MEM_SIZE;
// stack space: sp <= addr < bp

static ptr_t brk = memory;
// heap_space: memory <= addr < brk

#define PUSH(type, val) \
do { \
    sp -= sizeof(type); \
    *((type *)sp) = val; \
} while (0)

#define POP(type) (sp += sizeof(type))
#define PEEK(type) (*((type *)sp))

#define POP_N(type, n) (sp += (n) * sizeof(type))
#define PEEK_OFFSET(type, offset) \
    (*((type *)(sp + (offset) * sizeof(type))))

#define BOTTOM(type) (*((type *)(bp - sizeof(type))))

void exit_program(void) {
    fflush(stdout);
    exit(0);
}

/** node **/

#define NAPP    0
#define NGLOBAL 1
#define NIND    2
#define NDATA   3
#define NINT    4

#define FALSE 0
#define TRUE  1

struct _node;
typedef struct _node node_t;
typedef node_t *node_ptr_t;

struct _node {
    node_ptr_t gc_forwarding;
    int_t gc_blksize;
    byte_t gc_reachable; // dummy in free block
    byte_t gc_isglobal;  // dummy in free block
    byte_t type;
    union {
        struct { node_ptr_t left; node_ptr_t right; }; // application
        struct { int_t g_arity;   func_t code;      }; // global
        struct { node_ptr_t to;                     }; // indirection
        struct { int_t tag;       int_t d_arity;    }; // data
        struct { int_t intv;                        }; // int
    };
    node_ptr_t d_params[]; // flexible array for data node
};

#define NODE_SIZE(d_arity) (sizeof(node_t) + (d_arity) * sizeof(node_ptr_t))

#define GC_INIT_THRESHOLD 2048

int_t gc_threshold = GC_INIT_THRESHOLD;
int_t stat_gc_count = 0;
void global_gc(void);

// should call with proper value before each instruction
void mem_reserve(int_t r_size) {
    if ((ptr_t)brk - (ptr_t)memory >= gc_threshold || 
        (ptr_t)sp - (ptr_t)brk < r_size) {
        global_gc();
        if ((ptr_t)sp - (ptr_t)brk < r_size) {
            puts("Out of memory!");
            exit(1);
        }
        gc_threshold = 2 * ((ptr_t)brk - (ptr_t)memory);
    }
}

node_ptr_t node_alloc(int_t alloc_size) {
    node_ptr_t new_node = (node_ptr_t)brk;
    brk += alloc_size;

    new_node->gc_reachable = FALSE;
    new_node->gc_isglobal = FALSE;
    new_node->gc_blksize = alloc_size;

    return new_node;
}

void gc_mark(node_ptr_t p);

void gc_mark_children(node_ptr_t p) {
    switch (p->type) {
    case NAPP:
        gc_mark(p->left);
        gc_mark(p->right);
        break;
    case NIND:
        gc_mark(p->to);
        break;
    case NDATA:
        for (int_t i = p->d_arity - 1; i >= 0; --i) {
            gc_mark(p->d_params[i]);
        }
        break;
    default:
        break;
    }
}

void gc_mark(node_ptr_t p) {
    if (! p->gc_isglobal && ! p->gc_reachable) {
        p->gc_reachable = TRUE;
        gc_mark_children(p);
    }
}

void gc_update(node_ptr_t p) {
    switch (p->type) {
    case NAPP:
        p->left = p->left->gc_forwarding;
        p->right = p->right->gc_forwarding;
        break;
    case NIND:
        p->to = p->to->gc_forwarding;
        break;
    case NDATA:
        for (int_t i = p->d_arity - 1; i >= 0; --i) {
            p->d_params[i] = p->d_params[i]->gc_forwarding;
        }
        break;
    default:
        break;
    }
}

node_ptr_t globals;
int_t global_num;
void global_init(void);

// LISP2 GC algorithm
void global_gc(void) {
    // 1: mark
    for (int_t i = 0; i < global_num; ++i) {
        gc_mark_children(globals + i);
    }

    ptr_t _sp = sp;
    ptr_t _bp = bp;
    while (1) {
        while (sp < bp) {
            gc_mark(PEEK(node_ptr_t));
            POP(node_ptr_t);
        }
        if (bp == memory + MEM_SIZE)
            break;
        else {
            bp = PEEK(ptr_t);
            POP(ptr_t);
        }
    }
    sp = _sp;
    bp = _bp;

    // 2: set forwarding
    ptr_t new_position = (ptr_t)memory;
    node_ptr_t p = (node_ptr_t)memory;
    while ((ptr_t)p < (ptr_t)brk) {
        int_t blk_size = p->gc_blksize;
        if (p->gc_reachable) {
            p->gc_forwarding = (node_ptr_t)new_position;
            new_position = OFFSET(new_position, blk_size);
        }
        p = OFFSET(p, blk_size);
    }

    // 3: update forwarding
    for (int_t i = 0; i < global_num; ++i) {
        gc_update(globals + i);
    }

    _sp = sp;
    _bp = bp;
    while (1) {
        while (sp < bp) {
            PEEK(node_ptr_t) = PEEK(node_ptr_t)->gc_forwarding;
            POP(node_ptr_t);
        }
        if (bp == memory + MEM_SIZE)
            break;
        else {
            bp = PEEK(ptr_t);
            POP(ptr_t);
        }
    }
    sp = _sp;
    bp = _bp;

    p = (node_ptr_t)memory;
    while ((ptr_t)p < (ptr_t)brk) {
        if (p->gc_reachable) {
            gc_update(p);
        }
        p = OFFSET(p, p->gc_blksize);
    }
    
    // 4: move objects

    node_ptr_t new_p;
    p = (node_ptr_t)memory;
    while ((ptr_t)p < (ptr_t)brk) {
        int_t blk_size = p->gc_blksize;
        if (p->gc_reachable) {
            new_p = p->gc_forwarding;
            memmove(new_p, p, blk_size);
            new_p->gc_reachable = FALSE;
        }
        p = OFFSET(p, blk_size);
    }

    // 5: update brk
    brk = OFFSET(new_p, new_p->gc_blksize);
    
    stat_gc_count += 1;
}

void inst_pushg(int_t g_offset) {
    mem_reserve(sizeof(node_ptr_t));
    PUSH(node_ptr_t, globals + g_offset);
}

void inst_pushi(int_t val) {
    int_t h_alloc_size = NODE_SIZE(0);
    mem_reserve(h_alloc_size + sizeof(node_ptr_t));
    node_ptr_t p = node_alloc(h_alloc_size);
    p->type = NINT;
    p->intv = val;
    PUSH(node_ptr_t, p);
}

void inst_push(int_t offset) {
    mem_reserve(sizeof(node_ptr_t));
    node_ptr_t p = PEEK_OFFSET(node_ptr_t, offset);
    PUSH(node_ptr_t, p);
}

void inst_mkapp(void) {
    int_t h_alloc_size = NODE_SIZE(0);
    mem_reserve(h_alloc_size);
    node_ptr_t p = node_alloc(h_alloc_size);
    node_ptr_t p0 = PEEK_OFFSET(node_ptr_t, 0);
    node_ptr_t p1 = PEEK_OFFSET(node_ptr_t, 1);
    p->type = NAPP;
    p->left = p0; 
    p->right = p1;
    POP_N(node_ptr_t, 2);
    PUSH(node_ptr_t, p);
}

void inst_update(int_t offset) {
    node_ptr_t p = PEEK(node_ptr_t);
    POP(node_ptr_t);
    PEEK_OFFSET(node_ptr_t, offset)->type = NIND;
    PEEK_OFFSET(node_ptr_t, offset)->to = p;
}

void inst_pack(int_t tag, int_t arity) {
    int_t h_alloc_size = NODE_SIZE(arity);
    mem_reserve(h_alloc_size + sizeof(node_ptr_t));
    node_ptr_t p = node_alloc(h_alloc_size);
    p->type = NDATA;
    p->tag = tag;
    p->d_arity = arity;
    for (int_t i = 0; i < arity; ++i) {
        p->d_params[i] = PEEK(node_ptr_t);
        POP(node_ptr_t);
    }
    PUSH(node_ptr_t, p);
}

void inst_split(void) {
    node_ptr_t p = PEEK(node_ptr_t);
    int_t d_arity = p->d_arity;
    mem_reserve(d_arity * sizeof(node_ptr_t));
    POP(node_ptr_t);
    for (int_t i = d_arity - 1; i >= 0; --i) {
        PUSH(node_ptr_t, p->d_params[i]);
    }
}

void inst_slide(int_t n) {
    PEEK_OFFSET(node_ptr_t, n) = PEEK(node_ptr_t);
    POP_N(node_ptr_t, n);
}

void inst_unwind(void);

void inst_eval(void) {
    mem_reserve(sizeof(ptr_t));
    node_ptr_t p = PEEK(node_ptr_t);
    POP(node_ptr_t);
    PUSH(ptr_t, bp);
    bp = sp;
    PUSH(node_ptr_t, p);
    inst_unwind();
}

void inst_unwind(void) {
    while (1) {
        node_ptr_t p = PEEK(node_ptr_t);
        int_t arity;
        switch (p->type) {
        case NAPP: mem_reserve(sizeof(node_ptr_t)); PUSH(node_ptr_t, p->left); break;
        case NIND: PEEK(node_ptr_t) = p->to; break;
        case NGLOBAL:
            arity = p->g_arity;
            if (bp - sp >= (arity + 1) * sizeof(node_ptr_t)) {
                for (int_t i = 0; i < arity; ++i) {
                    PEEK_OFFSET(node_ptr_t, i) = PEEK_OFFSET(node_ptr_t, i + 1)->right;
                }
                p->code();
            } else {
                node_ptr_t bot = BOTTOM(node_ptr_t);
                sp = bp;
                bp = PEEK(ptr_t);
                POP(ptr_t);
                PUSH(node_ptr_t, bot);
                return;
            }
            break;
        default:
            sp = bp;
            bp = PEEK(ptr_t);
            POP(ptr_t);
            PUSH(node_ptr_t, p);
            return;
        }
    }
}

void inst_pop(int_t n) {
    POP_N(node_ptr_t, n);
}

#define INST_INT_ARITH_BINOP(op) \
    do { \
    int_t h_alloc_size = NODE_SIZE(0); \
    mem_reserve(h_alloc_size); \
    node_ptr_t p = node_alloc(h_alloc_size); \
    node_ptr_t p0 = PEEK_OFFSET(node_ptr_t, 0); \
    node_ptr_t p1 = PEEK_OFFSET(node_ptr_t, 1); \
    p->type = NINT; \
    p->intv = (p0->intv) op (p1->intv); \
    POP_N(node_ptr_t, 2); \
    PUSH(node_ptr_t, p); \
    } while (0) \

void inst_add(void) { INST_INT_ARITH_BINOP(+); }
void inst_sub(void) { INST_INT_ARITH_BINOP(-); }
void inst_mul(void) { INST_INT_ARITH_BINOP(*); }
void inst_div(void) { INST_INT_ARITH_BINOP(/); }
void inst_rem(void) { INST_INT_ARITH_BINOP(%); }

#define INST_INT_CMP_BINOP(op) \
    do { \
    int_t h_alloc_size = NODE_SIZE(0); \
    mem_reserve(h_alloc_size); \
    node_ptr_t p = node_alloc(h_alloc_size); \
    node_ptr_t p0 = PEEK_OFFSET(node_ptr_t, 0); \
    node_ptr_t p1 = PEEK_OFFSET(node_ptr_t, 1); \
    p->type = NDATA; \
    p->tag = (p0->intv) op (p1->intv) ? 1 : 0; \
    p->d_arity = 0; \
    POP_N(node_ptr_t, 2); \
    PUSH(node_ptr_t, p); \
    } while (0)

void inst_iseq(void) { INST_INT_CMP_BINOP(==); }
void inst_isgt(void) { INST_INT_CMP_BINOP(>); }
void inst_islt(void) { INST_INT_CMP_BINOP(<); }
void inst_isne(void) { INST_INT_CMP_BINOP(!=); }
void inst_isge(void) { INST_INT_CMP_BINOP(>=); }
void inst_isle(void) { INST_INT_CMP_BINOP(<=); }

void inst_not(void) {
    int_t h_alloc_size = NODE_SIZE(0);
    mem_reserve(h_alloc_size);
    node_ptr_t p = node_alloc(h_alloc_size);
    node_ptr_t p0 = PEEK(node_ptr_t);
    p->type = NDATA;
    p->tag = (! p0->tag) ? 1 : 0;
    p->d_arity = 0;
    POP(node_ptr_t);
    PUSH(node_ptr_t, p);
}

int_t entry_func_offset;

void print_head(const char *format) {
    inst_split();
    inst_eval();
    printf(format, PEEK(node_ptr_t)->intv);
    inst_pop(1);
    inst_eval();
}

void print_stat() {
    printf("GC: Count: %lld\n", stat_gc_count);
}

int main(void) {
    alloc_init();
    global_init();

    int_t input;
    scanf("%ld", &input);

    inst_pushi(input);
    inst_pushg(entry_func_offset);
    inst_mkapp();
    inst_eval();

    if (PEEK(node_ptr_t)->tag != 0) {
        print_head("%ld");
    }
    while (PEEK(node_ptr_t)->tag != 0) {
        print_head(",%ld");
    }
    putchar('\n');
    
    print_stat();

    exit_program();
}
