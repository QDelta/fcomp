#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

extern void exit(int);

typedef unsigned char u8_t;
typedef long long i64_t;
typedef unsigned short u16_t;
typedef unsigned int u32_t;
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

#define HALF_HEAP_SIZE (HEAP_SIZE >> 1)

static u8_t stack[STACK_SIZE];
static u8_t heap1[HALF_HEAP_SIZE];
static u8_t heap2[HALF_HEAP_SIZE];

static ptr_t bp = stack + STACK_SIZE;
static ptr_t sp = stack + STACK_SIZE;
// stack space: sp <= addr < bp

static ptr_t heap = heap1;
static ptr_t heap_to = heap2;
static ptr_t brk = heap1;
// heap_space: heap <= addr < brk

i64_t stat_max_stack_size = 0;

#define PUSH(type, val) \
do { \
    sp -= sizeof(type); \
    i64_t stk_size = (ptr_t)(stack + STACK_SIZE) - sp; \
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

#define INT_NODE(val) ((ptr_t)(((val) << 1) | 1))
#define IS_INT_NODE(ptr) ((i64_t)(ptr) & 1)
#define GET_INT_VAL(ptr) ((i64_t)(ptr) >> 1)

/** node **/

#define NAPP    0
#define NGLOBAL 1
#define NIND    2
#define NDATA   3
#define NNULL   4

#define FALSE 0
#define TRUE  1

struct _node;
typedef struct _node node_t;
typedef node_t *node_ptr_t;

// The size of a pointer must be 8 byte

// 63-bit integer node is represented
// in higher 63 bits of node_ptr_t
// with 1 on the lowest bit

struct _node {
    node_ptr_t gc_forwarding;
    u32_t gc_blksize;
    u8_t gc_copied; // dummy in free block
    u8_t gc_isglobal; // dummy in free block
    u8_t type;
    union {
        struct { node_ptr_t left; node_ptr_t right; }; // application
        struct { i64_t g_arity;   func_t code;      }; // global
        struct { node_ptr_t to;                     }; // indirection
        struct { i64_t tag;       i64_t d_arity;    }; // data
    };
    node_ptr_t d_params[]; // flexible array for data node
};

#define NODE_SIZE(d_arity) (sizeof(node_t) + (d_arity) * sizeof(node_ptr_t))

#define GC_INIT_THRESHOLD 4096

i64_t gc_threshold = GC_INIT_THRESHOLD;
i64_t stat_gc_count = 0;
i64_t stat_max_heap_size = 0;
clock_t stat_gc_clock = 0;
void global_gc(void);

node_ptr_t node_alloc(i64_t d_arity) {
    i64_t alloc_size = NODE_SIZE(d_arity);

    i64_t heap_size = (ptr_t)brk - (ptr_t)heap;
    i64_t new_heap_size = heap_size + alloc_size;
    if (heap_size >= gc_threshold
     || new_heap_size > HALF_HEAP_SIZE) {

        global_gc();

        heap_size = (ptr_t)brk - (ptr_t)heap;
        new_heap_size = heap_size + alloc_size;
        if (new_heap_size > HALF_HEAP_SIZE) {
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

    new_node->gc_copied = FALSE;
    new_node->gc_isglobal = FALSE;
    new_node->gc_blksize = alloc_size;

    return new_node;
}

node_ptr_t gc_copy(node_ptr_t p) {
    if (IS_INT_NODE(p) || p->gc_isglobal) {
        return p;
    }

    if (! p->gc_copied) {
        memcpy(brk, p, p->gc_blksize);
        p->gc_forwarding = brk;
        p->gc_copied = TRUE;
        brk += p->gc_blksize;
    }

    return p->gc_forwarding;
}

void gc_copy_children(node_ptr_t p) {
    switch (p->type) {
    case NAPP:
        p->left = gc_copy(p->left);
        p->right = gc_copy(p->right);
        break;
    case NIND:
        p->to = gc_copy(p->to);
        break;
    case NDATA:
        for (i64_t i = p->d_arity - 1; i >= 0; --i) {
            p->d_params[i] = gc_copy(p->d_params[i]);
        }
        break;
    default:
        break;
    }
}

node_ptr_t globals;
i64_t global_num;
void global_init(void);

// Cheney algorithm
void global_gc(void) {
    clock_t stat_gc_start_clock = clock();

    brk = heap_to;
    ptr_t scan = heap_to;

    // scan roots
    for (i64_t i = 0; i < global_num; ++i) {
        gc_copy_children(globals + i);
    }

    ptr_t _sp = sp;
    ptr_t _bp = bp;
    while (1) {
        while (sp < bp) {
            PEEK(node_ptr_t) = gc_copy(PEEK(node_ptr_t));
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

    // scan to-space
    while (scan < brk) {
        gc_copy_children(scan);
        scan += ((node_ptr_t)scan)->gc_blksize;
    }

    ptr_t tmp = heap;
    heap = heap_to;
    heap_to = tmp;

    stat_gc_count += 1;
    clock_t stat_gc_end_clock = clock();
    stat_gc_clock += stat_gc_end_clock - stat_gc_start_clock;
}

void inst_pushg(i64_t g_offset) {
    PUSH(node_ptr_t, globals + g_offset);
}

void inst_pushi(i64_t val) {
    node_ptr_t p = INT_NODE(val);
    PUSH(node_ptr_t, p);
}

void inst_push(i64_t offset) {
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

void inst_update(i64_t offset) {
    node_ptr_t p = PEEK(node_ptr_t);
    POP(node_ptr_t);
    PEEK_OFFSET(node_ptr_t, offset)->type = NIND;
    PEEK_OFFSET(node_ptr_t, offset)->to = p;
}

void inst_pack(i64_t tag, i64_t arity) {
    node_ptr_t p = node_alloc(arity);
    p->type = NDATA;
    p->tag = tag;
    p->d_arity = arity;
    for (i64_t i = 0; i < arity; ++i) {
        p->d_params[i] = PEEK(node_ptr_t);
        POP(node_ptr_t);
    }
    PUSH(node_ptr_t, p);
}

void inst_split(void) {
    node_ptr_t p = PEEK(node_ptr_t);
    i64_t d_arity = p->d_arity;
    POP(node_ptr_t);
    for (i64_t i = d_arity - 1; i >= 0; --i) {
        PUSH(node_ptr_t, p->d_params[i]);
    }
}

void inst_slide(i64_t n) {
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
        p = PEEK(node_ptr_t);
        if (IS_INT_NODE(p)) {
            goto eval_data;
        }

        i64_t arity;
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
                for (i64_t i = 0; i < arity; ++i) {
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
            goto eval_data;
        }
    }

eval_data:
    sp = bp;
    bp = PEEK(ptr_t);
    POP(ptr_t);
    PUSH(node_ptr_t, p);
}

void inst_pop(i64_t n) {
    POP_N(node_ptr_t, n);
}

void inst_alloc(i64_t n) {
    for (i64_t i = 0; i < n; ++i) {
        node_ptr_t p = node_alloc(0);
        p->type = NNULL;
        PUSH(node_ptr_t, p);
    }
}

#define INST_INT_ARITH_BINOP(op) \
    do { \
        node_ptr_t p0 = PEEK_OFFSET(node_ptr_t, 0); \
        node_ptr_t p1 = PEEK_OFFSET(node_ptr_t, 1); \
        node_ptr_t p = INT_NODE(GET_INT_VAL(p0) op GET_INT_VAL(p1)); \
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
        p->tag = GET_INT_VAL(p0) op GET_INT_VAL(p1) ? 1 : 0; \
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

i64_t entry_func_offset;

void print_head(const char *format) {
    inst_split();
    inst_eval();
    printf(format, GET_INT_VAL(PEEK(node_ptr_t)));
    inst_pop(1);
    inst_eval();
}

void print_stat() {
    fprintf(stderr, "GC count: %lld\n", stat_gc_count);
    fprintf(stderr, "GC threshold now: %lld bytes\n", gc_threshold);
    fprintf(stderr, "GC time: %.2f ms\n", 1000.0 * stat_gc_clock / CLOCKS_PER_SEC);
    fprintf(stderr, "Maximum stack usage: %lld bytes\n", stat_max_stack_size);
    fprintf(stderr, "Maximum heap usage: %lld bytes\n", stat_max_heap_size);
}

int main(void) {
    clock_t prog_start_clock = clock();

    global_init();

    i64_t input;
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
    fflush(stdout);

    clock_t prog_end_clock = clock();
    fprintf(stderr, "\n\nExecution time: %.2f ms\n", 1000.0 * (prog_end_clock - prog_start_clock) / CLOCKS_PER_SEC);
    print_stat();

    return 0;
}
