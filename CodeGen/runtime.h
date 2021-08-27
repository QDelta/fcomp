#ifdef FCOMP_RUNTIME
#else
#define FCOMP_RUNTIME

#include <stddef.h>

extern int printf(const char*, ...);
extern void free(void *);
extern void *malloc(size_t);
extern void *realloc(void *, size_t);

typedef unsigned uint_t;

struct _node;
typedef struct _node *addr_t;

typedef void (*func_t)(void);

typedef struct _node {
    enum {NApp, NGlobal, NInd, NData, NInt} type;
    union {
        // NApp
        struct { addr_t left; addr_t right; };
        // NGlobal
        struct { uint_t g_arity; func_t code; };
        // NInd
        struct { addr_t to; };
        // NData
        struct { uint_t tag; uint_t d_arity; addr_t* params; };
        // NInt
        struct { int intv; };
    };
} node_t;

#endif