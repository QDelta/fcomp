#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

extern void exit(int);

typedef unsigned char byte_t;
typedef long long int_t;
typedef void *ptr_t;
typedef void (*func_t)(void);

#define OFFSET(p, o) ((ptr_t)(p) + (o))

/** stack, heap **/

#ifndef STACK_SIZE
#define STACK_SIZE (1ULL << 20)
#endif

#ifndef HEAP_SIZE
#define HEAP_SIZE (1ULL << 24)
#endif

static byte_t stack[STACK_SIZE];
static byte_t heap[HEAP_SIZE];

static ptr_t bp = stack + STACK_SIZE;
static ptr_t sp = stack + STACK_SIZE;
// stack space: sp <= addr < bp

static ptr_t brk = heap;
// heap_space: memory <= addr < brk

int_t stat_max_stack_size = 0;

#define PUSH(type, val) \
do { \
    sp -= sizeof(type); \
    int_t stk_size = (ptr_t)(stack + STACK_SIZE) - sp; \
    if (sp < (ptr_t)stack) { \
        fputs("Stack overflow!\n", stderr); \
        exit(1); \
    } else if (stk_size > stat_max_stack_size) {\
        stat_max_stack_size = stk_size; \
    } \
    *((type *)sp) = val; \
} while (0)

#define POP(type) (sp += sizeof(type))
#define PEEK(type) (*((type *)sp))

#define POP_N(type, n) (sp += (n) * sizeof(type))
#define PEEK_OFFSET(type, offset) \
    (*((type *)(sp + (offset) * sizeof(type))))

#define BOTTOM(type) (*((type *)(bp - sizeof(type))))

/** node **/

#define NAPP    0
#define NGLOBAL 1
#define NIND    2
#define NDATA   3
#define NINT    4
#define NNULL   5

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

#define GC_INIT_THRESHOLD 4096

int_t gc_threshold = GC_INIT_THRESHOLD;
int_t stat_gc_count = 0;
int_t stat_max_heap_size = 0;
clock_t stat_gc_clock = 0;
void global_gc(void);

node_ptr_t node_alloc(int_t d_arity) {
    int_t alloc_size = NODE_SIZE(d_arity);

    int_t heap_size = (ptr_t)brk - (ptr_t)heap;
    int_t new_heap_size = heap_size + alloc_size;
    if (heap_size >= gc_threshold
     || new_heap_size > HEAP_SIZE) {

        global_gc();

        heap_size = (ptr_t)brk - (ptr_t)heap;
        new_heap_size = heap_size + alloc_size;
        if (new_heap_size > HEAP_SIZE) {
            fputs("Out of memory!\n", stderr);
            exit(1);
        }

        gc_threshold = 2 * heap_size;
    }

    if (new_heap_size > stat_max_heap_size) {
        stat_max_heap_size = new_heap_size;
    }

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
    clock_t stat_gc_start_clock = clock();

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
        if (bp == stack + STACK_SIZE)
            break;
        else {
            bp = PEEK(ptr_t);
            POP(ptr_t);
        }
    }
    sp = _sp;
    bp = _bp;

    // 2: set forwarding
    ptr_t new_position = (ptr_t)heap;
    node_ptr_t p = (node_ptr_t)heap;
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
        if (bp == stack + STACK_SIZE)
            break;
        else {
            bp = PEEK(ptr_t);
            POP(ptr_t);
        }
    }
    sp = _sp;
    bp = _bp;

    p = (node_ptr_t)heap;
    while ((ptr_t)p < (ptr_t)brk) {
        if (p->gc_reachable) {
            gc_update(p);
        }
        p = OFFSET(p, p->gc_blksize);
    }

    // 4: move objects
    node_ptr_t new_p;
    p = (node_ptr_t)heap;
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
    clock_t stat_gc_end_clock = clock();
    stat_gc_clock += stat_gc_end_clock - stat_gc_start_clock;
}

void inst_pushg(int_t g_offset) {
    PUSH(node_ptr_t, globals + g_offset);
}

void inst_pushi(int_t val) {
    node_ptr_t p = node_alloc(0);
    p->type = NINT;
    p->intv = val;
    PUSH(node_ptr_t, p);
}

void inst_push(int_t offset) {
    node_ptr_t p = PEEK_OFFSET(node_ptr_t, offset);
    PUSH(node_ptr_t, p);
}

void inst_mkapp(void) {
    node_ptr_t p = node_alloc(0);
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
    node_ptr_t p = node_alloc(arity);
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
    POP(node_ptr_t);
    for (int_t i = d_arity - 1; i >= 0; --i) {
        PUSH(node_ptr_t, p->d_params[i]);
    }
}

void inst_slide(int_t n) {
    PEEK_OFFSET(node_ptr_t, n) = PEEK(node_ptr_t);
    POP_N(node_ptr_t, n);
}

void inst_eval(void) {
    node_ptr_t p = PEEK(node_ptr_t);
    POP(node_ptr_t);
    PUSH(ptr_t, bp);
    bp = sp;
    PUSH(node_ptr_t, p);

    while (1) {
        node_ptr_t p = PEEK(node_ptr_t);
        int_t arity;
        switch (p->type) {
        case NAPP:
            PUSH(node_ptr_t, p->left);
            break;
        case NIND:
            PEEK(node_ptr_t) = p->to;
            break;
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

void inst_alloc(int_t n) {
    for (int_t i = 0; i < n; ++i) {
        node_ptr_t p = node_alloc(0);
        p->type = NNULL;
        PUSH(node_ptr_t, p);
    }
}

#define INST_INT_ARITH_BINOP(op) \
    do { \
        node_ptr_t p = node_alloc(0); \
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
        node_ptr_t p = node_alloc(0); \
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
    node_ptr_t p = node_alloc(0);
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
    printf("GC count: %lld\n", stat_gc_count);
    printf("GC threshold now: %lld bytes\n", gc_threshold);
    printf("GC time: %.2f ms\n", 1000.0 * stat_gc_clock / CLOCKS_PER_SEC);
    printf("Maximum stack usage: %lld bytes\n", stat_max_stack_size);
    printf("Maximum heap usage: %lld bytes\n", stat_max_heap_size);
}

int main(void) {
    clock_t prog_start_clock = clock();

    global_init();

    int_t input;
    scanf("%lld", &input);

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
    printf("\n\n");

    clock_t prog_end_clock = clock();
    printf("Execution time: %.2f ms\n", 1000.0 * (prog_end_clock - prog_start_clock) / CLOCKS_PER_SEC);
    print_stat();

    return 0;
}