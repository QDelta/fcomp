#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#define STACK_INIT_CAP 16
#define DUMP_INIT_CAP 16
#define GC_INIT_THRESHOLD 64

struct _node;
typedef struct _node node_t;
typedef node_t *node_ptr;

typedef void (*func_t)(void);

typedef unsigned char byte_t;

struct _node {
    union {
        struct { node_ptr left; node_ptr right; }; // application
        struct { int64_t g_arity; func_t code; }; // global
        struct { node_ptr to; }; // indirection
        struct { int64_t tag; int64_t d_arity; }; // data
        struct { int64_t intv; }; // int
    };
    node_ptr gc_next_node;
    node_ptr gc_forwarding;
    byte_t gc_copied;
    byte_t gc_isglobal;
    byte_t type;
    node_ptr d_params[]; // flexible array for data node
};

#define FALSE 0
#define TRUE  1

// node types
#define NAPP    0
#define NGLOBAL 1
#define NIND    2
#define NDATA   3
#define NINT    4

int64_t node_size(node_ptr n) {
    if (n->type == NDATA) {
        return sizeof(node_t) + n->d_arity * sizeof(node_ptr);
    } else {
        return sizeof(node_t);
    }
}

node_ptr node_list_head;
int64_t node_list_size;

int64_t stat_gc_count = 0;
int64_t stat_alloc_count = 0;

void global_gc(void);
int64_t gc_threshold;

int64_t global_func_num;
node_t *global_funcs;
void global_init(void); // will be generated

void heap_init(void) {
    node_list_head = NULL;
    node_list_size = 0;
    gc_threshold = GC_INIT_THRESHOLD;
}

node_ptr node_alloc(int64_t d_num) {
    int64_t alloc_size = sizeof(node_t) + d_num * sizeof(node_ptr);
    if (node_list_size >= gc_threshold) {
        global_gc();
        gc_threshold = node_list_size * 2;
    }

    node_ptr new_node = (node_ptr)malloc(alloc_size);
    new_node->gc_copied = FALSE;
    new_node->gc_forwarding = NULL;
    new_node->gc_isglobal = FALSE;
    new_node->gc_next_node = node_list_head;
    node_list_head = new_node;

    node_list_size += 1;
    stat_alloc_count += 1;

    return new_node;
}

void *free_node_list(void *head) {
    node_ptr p = (node_ptr)head;
    while (p) {
        node_ptr tmp = p->gc_next_node;
        free(p);
        p = tmp;
    }
    return NULL;
}

node_ptr *stack_arr;
int64_t stack_bp;
int64_t stack_sp;
int64_t stack_cap;

#define STACK_OFFSET(n) (stack_arr[stack_sp - 1 - (n)])
#define STACK_TOP (stack_arr[stack_sp - 1])

void stack_init(void) {
    stack_arr = malloc(STACK_INIT_CAP * sizeof(node_ptr));
    stack_bp = stack_sp = 0;
    stack_cap = STACK_INIT_CAP;
}

void stack_push(node_ptr n) {
    if (stack_sp == stack_cap) {
        stack_cap *= 2;
        stack_arr = realloc(stack_arr, stack_cap * sizeof(node_ptr));
    }
    stack_arr[stack_sp++] = n;
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
    free_node_list(node_list_head);
    exit(0);
}

node_ptr tospace_list_head;
int64_t tospace_list_size;

void gc_copy_children(node_ptr);

node_ptr gc_copy(node_ptr n) {
    if (! n || n->gc_isglobal)
        return n;
    else if (! n->gc_copied) {
        int64_t n_size = node_size(n);
        node_ptr forwarding = (node_ptr)malloc(n_size);
        memcpy(forwarding, n, n_size);

        n->gc_copied = TRUE;
        n->gc_forwarding = forwarding;

        forwarding->gc_copied = FALSE;
        forwarding->gc_forwarding = NULL;
        forwarding->gc_next_node = tospace_list_head;
        tospace_list_head = forwarding;

        tospace_list_size += 1;

        gc_copy_children(forwarding);
    }
    return n->gc_forwarding;
}

void gc_copy_children(node_ptr n) {
    switch (n->type) {
    case NAPP:
        n->left = gc_copy(n->left);
        n->right = gc_copy(n->right);
        break;
    case NIND:
        n->to = gc_copy(n->to);
        break;
    case NDATA:
        for (int64_t i = n->d_arity - 1; i >= 0; --i) {
            n->d_params[i] = gc_copy(n->d_params[i]);
        }
        break;
    default:
        break;
    }
}

pthread_t free_thrd;
byte_t thrd_activated = FALSE;

void global_gc(void) {
    if (thrd_activated) {
        pthread_join(free_thrd, NULL);
        thrd_activated = FALSE;
    }

    tospace_list_head = NULL;
    tospace_list_size = 0;

    for (int64_t i = stack_sp - 1; i >= 0; --i) {
        stack_arr[i] = gc_copy(stack_arr[i]);
    }
    for (int64_t i = global_func_num - 1; i >= 0; --i) {
        gc_copy_children(global_funcs + i);
    }

    thrd_activated = TRUE;
    pthread_create(&free_thrd, NULL, free_node_list, node_list_head);
    // free_node_list(node_list_head);

    node_list_head = tospace_list_head;
    node_list_size = tospace_list_size;

    stat_gc_count += 1;
}

void print_stat(void) {
    printf("Node: Allocation: %ld\n", stat_alloc_count);
    printf("Node: Count: %ld\n", node_list_size);
    printf("GC: Count: %ld\n", stat_gc_count);
}

void inst_pushg(int64_t g_offset) {
    stack_push(global_funcs + g_offset);
}

void inst_pushi(int64_t val) {
    node_ptr n = node_alloc(0);
    n->type = NINT;
    n->intv = val;
    stack_push(n);
}

void inst_push(int64_t offset) {
    stack_push(STACK_OFFSET(offset));
}

void inst_mkapp(void) {
    node_ptr n = node_alloc(0);
    node_ptr n0 = STACK_OFFSET(0);
    node_ptr n1 = STACK_OFFSET(1);
    n->type = NAPP;
    n->left = n0; n->right = n1;
    stack_sp -= 1;
    STACK_TOP = n;
}

void inst_update(int64_t offset) {
    node_ptr n = STACK_TOP;
    stack_sp -= 1;
    STACK_OFFSET(offset)->type = NIND;
    STACK_OFFSET(offset)->to = n;
}

void inst_pack(int64_t tag, int64_t arity) {
    node_ptr n = node_alloc(arity);
    n->type = NDATA;
    n->tag = tag; n->d_arity = arity;
    for (int i = 0; i < arity; ++i) {
        n->d_params[i] = STACK_OFFSET(i);
    }
    stack_sp -= arity;
    stack_push(n);
}

void inst_split(void) {
    node_ptr n = STACK_TOP;
    stack_sp -= 1;
    for (int i = n->d_arity - 1; i >= 0; --i) {
        stack_push(n->d_params[i]);
    }
}

void inst_slide(int64_t n) {
    STACK_OFFSET(n) = STACK_TOP;
    stack_sp -= n;
}

void inst_unwind(void);

void inst_eval(void) {
    dump_push(stack_bp);
    stack_bp = stack_sp - 1;
    inst_unwind();
}

void inst_unwind(void) {
    while (1) {
        node_ptr n = STACK_TOP;
        int arity;
        switch (n->type) {
            case NAPP: stack_push(n->left); break;
            case NIND: STACK_TOP = n->to; break;
            case NGLOBAL:
                arity = n->g_arity;
                if (stack_sp - stack_bp - 1 >= arity) {
                    for (int i = 0; i < arity; ++i) {
                        STACK_OFFSET(i) = STACK_OFFSET(i + 1)->right;
                    }
                    n->code();
                } else {
                    stack_sp = stack_bp + 1;
                    stack_bp = DUMP_TOP;
                    dump_sp -= 1;
                    return;
                }
                break;
            default:
                stack_arr[stack_bp] = n;
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
//         node_ptr n = node_alloc(0);
//         n->type = NIND;
//         n->to = NULL;
//         stack_push(n);
//     }
// }

#define INST_INT_ARITH_BINOP(op) \
    do { \
    node_ptr n = node_alloc(0); \
    node_ptr n0 = STACK_OFFSET(0); \
    node_ptr n1 = STACK_OFFSET(1); \
    n->type = NINT; \
    n->intv = (n0->intv) op (n1->intv); \
    stack_sp -= 1; \
    STACK_TOP = n; \
    } while (0)

void inst_add(void) { INST_INT_ARITH_BINOP(+); }
void inst_sub(void) { INST_INT_ARITH_BINOP(-); }
void inst_mul(void) { INST_INT_ARITH_BINOP(*); }
void inst_div(void) { INST_INT_ARITH_BINOP(/); }
void inst_rem(void) { INST_INT_ARITH_BINOP(%); }

#define INST_INT_CMP_BINOP(op) \
    do { \
    node_ptr n = node_alloc(0); \
    node_ptr n0 = STACK_OFFSET(0); \
    node_ptr n1 = STACK_OFFSET(1); \
    n->type = NDATA; \
    n->tag = (n0->intv) op (n1->intv) ? 1 : 0; \
    n->d_arity = 0; \
    stack_sp -= 1; \
    STACK_TOP = n; \
    } while (0)

void inst_iseq(void) { INST_INT_CMP_BINOP(==); }
void inst_isgt(void) { INST_INT_CMP_BINOP(>); }
void inst_islt(void) { INST_INT_CMP_BINOP(<); }
void inst_isne(void) { INST_INT_CMP_BINOP(!=); }
void inst_isge(void) { INST_INT_CMP_BINOP(>=); }
void inst_isle(void) { INST_INT_CMP_BINOP(<=); }

void inst_not(void) {
    node_ptr n = node_alloc(0);
    node_ptr n0 = STACK_TOP;
    n->type = NDATA;
    n->tag = (! n0->tag) ? 1 : 0;
    n->d_arity = 0;
    STACK_TOP = n;
}

int64_t entry_func_offset;

void print_head(const char* format) {
    inst_split();
    inst_eval();
    printf(format, STACK_TOP->intv);
    inst_pop(1);
    inst_eval();
}

int main(void) {
    heap_init();
    stack_init();
    dump_init();

    global_init();

    int64_t input;
    scanf("%ld", &input);
    
    inst_pushi(input);
    inst_pushg(entry_func_offset);
    inst_mkapp();
    inst_eval();

    if (STACK_TOP->tag != 0) {
        print_head("%d");
    }
    while (STACK_TOP->tag != 0) {
        print_head(",%d");
    }
    putchar('\n');

    print_stat();

    exit_program();
}
