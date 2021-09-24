#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define STACK_INIT_CAP 16
#define DUMP_INIT_CAP 16
#define SLAB_INIT_CAP 128
#define GC_INIT_THRESHOLD 64

typedef unsigned char byte_t;
typedef int64_t addr_t;

typedef void (*func_t)(void);

typedef struct {
    union {
        struct { addr_t left; addr_t right; }; // application
        struct { int64_t g_arity; func_t code; }; // global
        struct { addr_t to; }; // indirection
        struct { int64_t tag; int64_t d_arity; addr_t *d_params; }; // data
        struct { int64_t intv; }; // int
    };
    byte_t type;
    byte_t gc_reachable;
    byte_t gc_isglobal;
} node_t;

#define FALSE 0
#define TRUE  1

// node types
#define NAPP    0
#define NGLOBAL 1
#define NIND    2
#define NDATA   3
#define NINT    4

// free allocated memory in a node
void free_node(node_t n) {
    if (n.type == NDATA) {
        free(n.d_params);
    }
}

void global_gc(void);
void exit_program(void);

typedef struct {
    union {
        int64_t next_vacant;
        node_t node;
    };
    byte_t slot_tag;
} slot_t;

// slot tags
#define VACANT   0
#define OCCUPIED 1

slot_t *slab_arr = NULL;
int64_t slab_first_vacant_id;
int64_t slab_size;
int64_t slab_cap;
int64_t slab_occupied_count;
int64_t slab_gc_threshold;

int64_t stat_gc_count = 0;
int64_t stat_alloc_count = 0;
int64_t stat_free_count = 0;

void slab_init(void) {
    slab_arr = malloc(SLAB_INIT_CAP * sizeof(slot_t));
    slab_first_vacant_id = slab_size = 0;
    slab_cap = SLAB_INIT_CAP;
    slab_occupied_count = 0;
    slab_gc_threshold = SLAB_INIT_CAP;
}

addr_t slab_alloc(void) {
    addr_t new_addr;

    if (slab_size == slab_first_vacant_id) {
        if (slab_size == slab_cap) {
            slab_cap *= 2;
            slab_arr = realloc(slab_arr, slab_cap * sizeof(slot_t));
        }
        slab_size += 1;
        new_addr = slab_first_vacant_id;
        slab_first_vacant_id += 1;
    } else {
        slot_t v_slot = slab_arr[slab_first_vacant_id];
        new_addr = slab_first_vacant_id;
        slab_first_vacant_id = v_slot.next_vacant;
    }

    slab_arr[new_addr].slot_tag = OCCUPIED;
    slab_occupied_count += 1;

    stat_alloc_count += 1;
    return new_addr;
}

void slab_free(addr_t a) {
    free_node(slab_arr[a].node);
    slab_arr[a].slot_tag = VACANT;
    slab_arr[a].next_vacant = slab_first_vacant_id;
    slab_first_vacant_id = a;
    slab_occupied_count -= 1;

    stat_free_count += 1;
}

void slab_destroy(void) {
    for (int64_t i = 0; i < slab_size; ++i) {
        if (slab_arr[i].slot_tag == OCCUPIED) {
            free_node(slab_arr[i].node);
        }
    }
    free(slab_arr);
}

#define NODE(a) (&(slab_arr[a].node))

addr_t node_alloc_nogc(void) {
    addr_t a = slab_alloc();
    NODE(a)->gc_reachable = FALSE;
    NODE(a)->gc_isglobal = FALSE;
    return a;
}

addr_t node_alloc(void) {
    if (slab_occupied_count >= slab_gc_threshold) {
        global_gc();
        slab_gc_threshold = slab_occupied_count * 2;
    }
    return node_alloc_nogc();
}

void global_init(void);

addr_t *stack_arr = NULL;
int64_t stack_bp;
int64_t stack_sp;
int64_t stack_cap;

#define STACK_OFFSET(n) (stack_arr[stack_sp - 1 - (n)])
#define STACK_TOP (stack_arr[stack_sp - 1])

void stack_init(void) {
    stack_arr = malloc(STACK_INIT_CAP * sizeof(addr_t));
    stack_bp = stack_sp = 0;
    stack_cap = STACK_INIT_CAP;
}

void stack_push(addr_t a) {
    if (stack_sp == stack_cap) {
        stack_cap *= 2;
        stack_arr = realloc(stack_arr, stack_cap * sizeof(addr_t));
    }
    stack_arr[stack_sp++] = a;
}

int64_t *dump_arr;
int64_t dump_sp;
int64_t dump_cap;

#define DUMP_TOP (dump_arr[dump_sp - 1])

void dump_init(void) {
    dump_arr = malloc(DUMP_INIT_CAP * sizeof(int64_t));
    dump_sp = 0;
    dump_cap = DUMP_INIT_CAP;
}

void dump_push(int64_t bp) {
    if (dump_sp == dump_cap) {
        dump_cap *= 2;
        dump_arr = realloc(dump_arr, dump_cap * sizeof(int64_t));
    }
    dump_arr[dump_sp++] = bp;
}

void exit_program(void) {
    fflush(stderr);
    fflush(stdout);
    free(stack_arr);
    free(dump_arr);
    slab_destroy();
    exit(0);
}


// void stack_traverse(void (*f)(addr_t)) {
//     int64_t sp = stack_sp;
//     int64_t bp = stack_bp;
//     while (bp > 0) {
//         for (int64_t i = sp - 1; i >= bp; --i) {
//             f(stack_arr[i]);
//         }
//         sp = bp - 1;
//         bp = (int64_t)stack_arr[sp];
//     }
//     for (int64_t i = sp - 1; i >= 0; --i) {
//         f(stack_arr[i]);
//     }
// }

void gc_track_children(addr_t a);

void gc_track(addr_t a) {
    if (! NODE(a)->gc_isglobal && ! NODE(a)->gc_reachable) {
        NODE(a)->gc_reachable = TRUE;
        gc_track_children(a);
    }
}

void gc_track_children(addr_t a) {
    switch (NODE(a)->type) {
    case NAPP:
        gc_track(NODE(a)->left);
        gc_track(NODE(a)->right);
        break;
    case NIND:
        gc_track(NODE(a)->to);
        break;
    case NDATA:
        for (int64_t i = NODE(a)->d_arity - 1; i >= 0; --i) {
            gc_track(NODE(a)->d_params[i]);
        }
        break;
    default: break;
    }
}

void global_gc(void) {
    // stack_traverse(gc_track);
    for (int64_t i = stack_sp - 1; i >= 0; --i) {
        gc_track(stack_arr[i]);
    }
    for (int64_t i = slab_size - 1; i >= 0; --i) {
        if (slab_arr[i].slot_tag == OCCUPIED 
            && ! slab_arr[i].node.gc_isglobal) {
            if (! slab_arr[i].node.gc_reachable)
                slab_free(i);
            else
                slab_arr[i].node.gc_reachable = FALSE;
        }
    }
    stat_gc_count += 1;
}

void print_stat(void) {
    printf("Slab: Alloc count: %ld\n", stat_alloc_count);
    printf("Slab: Free count: %ld\n", stat_free_count);
    printf("Slab: Now size: %ld\n", slab_size);
    printf("Slab: Now capacity: %ld\n", slab_cap);
    printf("Slab: Now occupied: %ld\n", slab_occupied_count);
    printf("GC: Count: %ld\n", stat_gc_count);
    printf("GC: Now threshold: %ld\n", slab_gc_threshold);
}

void inst_pushg(addr_t p) {
    stack_push(p);
}

void inst_pushi(int64_t val) {
    addr_t a = node_alloc();
    NODE(a)->type = NINT;
    NODE(a)->intv = val;
    stack_push(a);
}

void inst_push(int64_t offset) {
    stack_push(STACK_OFFSET(offset));
}

void inst_mkapp(void) {
    addr_t a = node_alloc();
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    NODE(a)->type = NAPP;
    NODE(a)->left = a0; NODE(a)->right = a1;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_update(int64_t offset) {
    addr_t a = STACK_TOP;
    stack_sp -= 1;
    NODE(STACK_OFFSET(offset))->type = NIND;
    NODE(STACK_OFFSET(offset))->to = a;
}

void inst_pack(int64_t tag, int64_t arity) {
    addr_t a = node_alloc();
    NODE(a)->type = NDATA;
    NODE(a)->tag = tag; NODE(a)->d_arity = arity;
    NODE(a)->d_params = arity ? malloc(arity * sizeof(addr_t)) : NULL;
    for (int64_t i = 0; i < arity; ++i) {
        NODE(a)->d_params[i] = STACK_OFFSET(i);
    }
    stack_sp -= arity;
    stack_push(a);
}

void inst_split(void) {
    addr_t a = STACK_TOP;
    stack_sp -= 1;
    for (int64_t i = NODE(a)->d_arity - 1; i >= 0; --i) {
        stack_push(NODE(a)->d_params[i]);
    }
}

void inst_slide(int64_t n) {
    STACK_OFFSET(n) = STACK_TOP;
    stack_sp -= n;
}

void inst_unwind(void);

void inst_eval(void) {
    // addr_t a = STACK_TOP;
    // STACK_TOP = (addr_t)stack_bp;
    // stack_bp = stack_sp;
    // stack_push(a);
    dump_push(stack_bp);
    stack_bp = stack_sp - 1;
    inst_unwind();
}

void inst_unwind(void) {
    while (1) {
        addr_t a = STACK_TOP;
        int64_t arity;
        switch (NODE(a)->type) {
            case NAPP: stack_push(NODE(a)->left); break;
            case NIND: STACK_TOP = NODE(a)->to; break;
            case NGLOBAL:
                arity = NODE(a)->g_arity;
                if (stack_sp - stack_bp - 1 >= arity) {
                    for (int64_t i = 0; i < arity; ++i) {
                        STACK_OFFSET(i) = NODE(STACK_OFFSET(i + 1))->right;
                    }
                    NODE(a)->code();
                } else {
                    // stack_sp = stack_bp;
                    // stack_bp = (int64_t)STACK_TOP;
                    // STACK_TOP = stack_arr[stack_sp];
                    stack_sp = stack_bp + 1;
                    stack_bp = DUMP_TOP;
                    dump_sp -= 1;
                    return;
                }
                break;
            default:
                // stack_sp = stack_bp;
                // stack_bp = (int64_t)STACK_TOP;
                // STACK_TOP = a;
                stack_arr[stack_bp] = a;
                stack_sp = stack_bp + 1;
                stack_bp = DUMP_TOP;
                dump_sp -= 1;
                return;
        }
    }
}

void inst_pop(int64_t n) {
    stack_sp -= n;
}

// void inst_alloc(int64_t n) {
//     for (int64_t i = 0; i < n; ++i) {
//         addr_t a = node_alloc();
//         NODE(a)->type = NIND;
//         NODE(a)->to = -1;
//         stack_push(a);
//     }
// }

#define INST_INT_ARITH_BINOP(op) \
    do { \
    addr_t a = node_alloc(); \
    addr_t a0 = STACK_OFFSET(0); \
    addr_t a1 = STACK_OFFSET(1); \
    NODE(a)->type = NINT; \
    NODE(a)->intv = (NODE(a0)->intv) op (NODE(a1)->intv); \
    stack_sp -= 1; \
    STACK_TOP = a; \
    } while (0)

void inst_add(void) { INST_INT_ARITH_BINOP(+); }
void inst_sub(void) { INST_INT_ARITH_BINOP(-); }
void inst_mul(void) { INST_INT_ARITH_BINOP(*); }
void inst_div(void) { INST_INT_ARITH_BINOP(/); }
void inst_rem(void) { INST_INT_ARITH_BINOP(%); }

#define INST_INT_CMP_BINOP(op) \
    do { \
    addr_t a = node_alloc(); \
    addr_t a0 = STACK_OFFSET(0); \
    addr_t a1 = STACK_OFFSET(1); \
    NODE(a)->type = NDATA; \
    NODE(a)->tag = (NODE(a0)->intv) op (NODE(a1)->intv) ? 1 : 0; \
    NODE(a)->d_params = NULL; NODE(a)->d_arity = 0; \
    stack_sp -= 1; \
    STACK_TOP = a; \
    } while (0)

void inst_iseq(void) { INST_INT_CMP_BINOP(==); }
void inst_isgt(void) { INST_INT_CMP_BINOP(>); }
void inst_islt(void) { INST_INT_CMP_BINOP(<); }
void inst_isne(void) { INST_INT_CMP_BINOP(!=); }
void inst_isge(void) { INST_INT_CMP_BINOP(>=); }
void inst_isle(void) { INST_INT_CMP_BINOP(<=); }

void inst_not(void) {
    addr_t a = node_alloc();
    addr_t a0 = STACK_TOP;
    NODE(a)->type = NDATA;
    NODE(a)->tag = (! NODE(a0)->tag) ? 1 : 0;
    NODE(a)->d_params = NULL; NODE(a)->d_arity = 0;
    STACK_TOP = a;
}

addr_t entry_func_addr;

void print_head(const char* format) {
    inst_split();
    inst_eval();
    printf(format, NODE(STACK_TOP)->intv);
    inst_pop(1);
    inst_eval();
}

int main(void) {
    slab_init();
    stack_init();
    dump_init();

    global_init();

    int64_t input;
    scanf("%ld", &input);
    
    inst_pushi(input);
    inst_pushg(entry_func_addr);
    inst_mkapp();
    inst_eval();

    if (NODE(STACK_TOP)->tag != 0) {
        print_head("%ld");
    }
    while (NODE(STACK_TOP)->tag != 0) {
        print_head(",%ld");
    }
    putchar('\n');

    print_stat();

    exit_program();
}
