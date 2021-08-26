#include <stddef.h>

extern void free(void *);
extern void *malloc(size_t);
extern void *realloc(void *, size_t);

typedef unsigned uint_t;

struct _node;
typedef struct _node *addr_t;

typedef struct {
    addr_t *arr;
    size_t size, cap;
} addr_vec_t;

void addr_vec_init(addr_vec_t *v) {
    v->arr = (addr_t *)malloc(16 * sizeof(addr_t));
    v->size = 0;
    v->cap = 16;
}

void addr_vec_free(addr_vec_t *v) {
    free(v->arr);
}

void addr_vec_push(addr_vec_t *v, addr_t a) {
    if (v->size == v->cap) {
        v->cap *= 2;
        v->arr = (addr_t *)realloc(v->arr, v->cap);
    }
    v->arr[v->size] = a;
    v->size += 1;
}

typedef struct {
    addr_vec_t vec;
    size_t bp;
} stack_t;

typedef void (*func_t)(stack_t *);

typedef struct _node {
    enum {NApp, NGlobal, NInd, NData, NInt} type;
    union {
        struct { addr_t left; addr_t right; };      // NApp
        struct { uint_t arity; func_t code; };      // NGlobal
        struct { addr_t to; };                      // NInd
        struct { uint_t tag; addr_vec_t params; };  // NData
        struct { int intv; };                       // NInt
    };
} node_t;

void free_node(node_t *p) {
    if (p->type == NData) {
        addr_vec_free(&p->params);
    }
    free(p);
}

void stack_init(stack_t *s) {
    addr_vec_init(&s->vec);
    s->bp = 0;
}

void stack_free(stack_t *s) {
    addr_vec_free(&s->vec);
}

void stack_push(stack_t *s, addr_t a) {
    addr_vec_push(&s->vec, a);
}

void stack_push_offset(stack_t *s, uint_t offset) {
    addr_vec_push(&s->vec, s->vec.arr[s->vec.size - 1 - offset]);
}



