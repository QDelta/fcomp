#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STACK_INIT_CAP 16
#define SLAB_INIT_CAP 64

typedef unsigned uint_t;

typedef size_t addr_t;

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
    enum {ALIVE, UNTRACKED} gc_tag;
} node_t;

// free allocated memory in a node
void free_node(node_t n) {
    if (n.type == NData) {
        free(n.params);
    }
}

void global_gc(void);
void exit_program(void);

typedef struct {
    enum {Vacant, Occupied} slot_tag;
    union {
        size_t next_vacant;
        node_t node;
    };
} slot_t;

slot_t *slab_arr = NULL;
size_t slab_first_vacant_id;
size_t slab_size;
size_t slab_cap;
size_t slab_occupied_count;
size_t slab_gc_threshold;

void slab_init(void) {
    slab_arr = malloc(SLAB_INIT_CAP * sizeof(slot_t));
    slab_first_vacant_id = slab_size = 0;
    slab_cap = SLAB_INIT_CAP;
    slab_occupied_count = 0;
    slab_gc_threshold = SLAB_INIT_CAP;
}

addr_t slab_alloc(void) {
    if (slab_occupied_count >= slab_gc_threshold) {
        global_gc();
        slab_gc_threshold = slab_occupied_count * 2;
    }

    size_t new_addr;

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

    slab_arr[new_addr].slot_tag = Occupied;
    slab_occupied_count += 1;
    return new_addr;
}

void slab_free(addr_t a) {
    free_node(slab_arr[a].node);
    slab_arr[a].slot_tag = Vacant;
    slab_arr[a].next_vacant = slab_first_vacant_id;
    slab_first_vacant_id = a;
    slab_occupied_count -= 1;
}

void slab_destroy(void) {
    for (size_t i = 0; i < slab_size; ++i) {
        if (slab_arr[i].slot_tag == Occupied) {
            free_node(slab_arr[i].node);
        }
    }
    free(slab_arr);
}

node_t *mem(addr_t a) {
    return slab_arr[a].slot_tag == Occupied ? &(slab_arr[a].node) : NULL;
}

addr_t mem_alloc(void) {
    return slab_alloc();
}

void global_init();

addr_t *stack_arr = NULL;
size_t stack_bp;
size_t stack_sp;
size_t stack_cap;

#define STACK_OFFSET(n) (stack_arr[stack_sp - 1 - (n)])
#define STACK_TOP (stack_arr[stack_sp - 1])

void stack_init(void) {
    stack_arr = malloc(STACK_INIT_CAP * sizeof(addr_t));
    stack_bp = stack_sp = 0;
    stack_cap = STACK_INIT_CAP;
}

void stack_free(void) {
    free(stack_arr);
    stack_arr = NULL;
    stack_bp = stack_sp = stack_cap = 0;
}

void exit_program(void) {
    fflush(stdout);
    fflush(stderr);
    stack_free();
    slab_destroy();
    exit(0);
}

void stack_push(addr_t a) {
    if (stack_sp == stack_cap) {
        stack_cap *= 2;
        stack_arr = realloc(stack_arr, stack_cap * sizeof(addr_t));
    }
    stack_arr[stack_sp++] = a;
}

void stack_traverse(void (*f)(addr_t)) {
    long sp = stack_sp;
    long bp = stack_bp;
    while (bp > 0) {
        for (long i = sp - 1; i >= bp; --i) {
            f(stack_arr[i]);
        }
        sp = bp - 1;
        bp = (size_t)stack_arr[sp];
    }
    for (long i = sp - 1; i >= 0; --i) {
        f(stack_arr[i]);
    }
}

void gc_track(addr_t a) {
    if (mem(a)->type != NGlobal && mem(a)->gc_tag == UNTRACKED) {
        mem(a)->gc_tag = ALIVE;
        switch (mem(a)->type) {
            case NApp:
                gc_track(mem(a)->left);
                gc_track(mem(a)->right);
                break;
            case NInd:
                gc_track(mem(a)->to);
                break;
            case NData:
                for (int i = mem(a)->d_arity - 1; i >= 0; --i) {
                    gc_track(mem(a)->params[i]);
                }
                break;
            default: break;
        }
    }
}

void global_gc(void) {
    for (long i = 0; i < slab_size; ++i) {
        if (slab_arr[i].slot_tag == Occupied 
            && slab_arr[i].node.type != NGlobal) {
            slab_arr[i].node.gc_tag = UNTRACKED;
        }
    }
    stack_traverse(gc_track);
    for (long i = 0; i < slab_size; --i) {
        if (slab_arr[i].slot_tag == Occupied 
            && slab_arr[i].node.type != NGlobal 
            && slab_arr[i].node.gc_tag == UNTRACKED) {
            slab_free(i);
        }
    }
}

void inst_pushg(addr_t p) {
    stack_push(p);
}

void inst_pushi(int val) {
    addr_t a = mem_alloc();
    mem(a)->type = NInt;
    mem(a)->intv = val;
    stack_push(a);
}

void inst_push(uint_t offset) {
    stack_push(STACK_OFFSET(offset));
}

void inst_mkapp(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NApp;
    mem(a)->left = a0; mem(a)->right = a1;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_update(uint_t offset) {
    addr_t a = STACK_TOP;
    stack_sp -= 1;
    mem(STACK_OFFSET(offset))->type = NInd;
    mem(STACK_OFFSET(offset))->to = a;
}

void inst_pack(uint_t tag, uint_t arity) {
    addr_t a = mem_alloc();
    mem(a)->type = NData;
    mem(a)->tag = tag; mem(a)->d_arity = arity;
    mem(a)->params = arity ? malloc(arity * sizeof(addr_t)) : NULL;
    for (int i = 0; i < arity; ++i) {
        mem(a)->params[i] = STACK_OFFSET(i);
    }
    stack_sp -= arity;
    stack_push(a);
}

void inst_split(void) {
    addr_t a = STACK_TOP;
    stack_sp -= 1;
    for (int i = mem(a)->d_arity - 1; i >= 0; --i) {
        stack_push(mem(a)->params[i]);
    }
}

void inst_slide(uint_t n) {
    STACK_OFFSET(n) = STACK_TOP;
    stack_sp -= n;
}

void inst_unwind(void);

void inst_eval(void) {
    addr_t a = STACK_TOP;
    STACK_TOP = (addr_t)stack_bp;
    stack_bp = stack_sp;
    stack_push(a);
    inst_unwind();
}

void inst_unwind(void) {
    while (1) {
        addr_t a = STACK_TOP;
        uint_t arity;
        switch (mem(a)->type) {
            case NApp: stack_push(mem(a)->left); break;
            case NInd: STACK_TOP = mem(a)->to; break;
            case NGlobal:
                arity = mem(a)->g_arity;
                if (stack_sp - stack_bp - 1 >= arity) {
                    for (int i = 0; i < arity; ++i) {
                        STACK_OFFSET(i) = mem(STACK_OFFSET(i + 1))->right;
                    }
                    mem(a)->code();
                } else {
                    stack_sp = stack_bp;
                    stack_bp = (size_t)STACK_TOP;
                    STACK_TOP = stack_arr[stack_sp];
                    return;
                }
                break;
            default:
                stack_sp = stack_bp;
                stack_bp = (size_t)STACK_TOP;
                STACK_TOP = a;
                return;
        }
    }
}

void inst_pop(uint_t n) {
    stack_sp -= n;
}

void inst_alloc(uint_t n) {
    for (int i = 0; i < n; ++i) {
        addr_t a = mem_alloc();
        mem(a)->type = NInd;
        mem(a)->to = -1;
        stack_push(a);
    }
}

void inst_add(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NInt;
    mem(a)->intv = mem(a0)->intv + mem(a1)->intv;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_sub(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NInt;
    mem(a)->intv = mem(a0)->intv - mem(a1)->intv;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_mul(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NInt;
    mem(a)->intv = mem(a0)->intv * mem(a1)->intv;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_div(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NInt;
    mem(a)->intv = mem(a0)->intv / mem(a1)->intv;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_rem(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NInt;
    mem(a)->intv = mem(a0)->intv % mem(a1)->intv;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_iseq(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NData;
    mem(a)->tag = mem(a0)->intv == mem(a1)->intv ? 1 : 0;
    mem(a)->params = NULL; mem(a)->d_arity = 0;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_isgt(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NData;
    mem(a)->tag = mem(a0)->intv > mem(a1)->intv ? 1 : 0;
    mem(a)->params = NULL; mem(a)->d_arity = 0;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_islt(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NData;
    mem(a)->tag = mem(a0)->intv < mem(a1)->intv ? 1 : 0;
    mem(a)->params = NULL; mem(a)->d_arity = 0;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_and(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NData;
    mem(a)->tag = mem(a0)->tag && mem(a1)->tag ? 1 : 0;
    mem(a)->params = NULL; mem(a)->d_arity = 0;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_or(void) {
    addr_t a0 = STACK_OFFSET(0);
    addr_t a1 = STACK_OFFSET(1);
    addr_t a = mem_alloc();
    mem(a)->type = NData;
    mem(a)->tag = mem(a0)->tag || mem(a1)->tag ? 1 : 0;
    mem(a)->params = NULL; mem(a)->d_arity = 0;
    stack_sp -= 1;
    STACK_TOP = a;
}

void inst_not(void) {
    addr_t a0 = STACK_TOP;
    addr_t a = mem_alloc();
    mem(a)->type = NData;
    mem(a)->tag = (! mem(a0)->tag) ? 1 : 0;
    mem(a)->params = NULL; mem(a)->d_arity = 0;
    STACK_TOP = a;
}

addr_t main_func_addr;

int main(void) {
    slab_init();
    stack_init();

    global_init();

    int input;
    scanf("%d", &input);
    
    inst_pushi(input);
    inst_pushg(main_func_addr);
    inst_mkapp();
    inst_eval();
    while (mem(STACK_TOP)->tag != 0) {
        inst_split();
        inst_eval();
        printf("%d,", mem(STACK_TOP)->intv);
        inst_pop(1);
        inst_eval();
    }
    
    exit_program();
}
void ff_ife(void) {
inst_push(0);
inst_eval();
switch (mem(STACK_TOP)->tag) {
case 0:
inst_split();
inst_push(2);
inst_eval();
inst_slide(0);
break;
case 1:
inst_split();
inst_push(1);
inst_eval();
inst_slide(0);
break;
default: fprintf(stderr, "Non-exhaustive pattern"); exit_program();
};
inst_slide(4);
}
void ff_from(void) {
inst_push(0);
inst_pushi(1);
inst_pushg(20);
inst_mkapp();
inst_mkapp();
inst_pushg(1);
inst_mkapp();
inst_push(1);
inst_pushg(21);
inst_mkapp();
inst_mkapp();
inst_eval();
inst_slide(2);
}
void ff_notDivide(void) {
inst_push(0);
inst_eval();
inst_push(2);
inst_eval();
inst_rem();
inst_pushi(0);
inst_iseq();
inst_not();
inst_slide(3);
}
void ff_filter(void) {
inst_push(1);
inst_eval();
switch (mem(STACK_TOP)->tag) {
case 0:
inst_split();
inst_pushg(22);
inst_eval();
inst_slide(0);
break;
case 1:
inst_split();
inst_push(1);
inst_push(3);
inst_pushg(3);
inst_mkapp();
inst_mkapp();
inst_push(2);
inst_push(4);
inst_pushg(3);
inst_mkapp();
inst_mkapp();
inst_push(2);
inst_pushg(21);
inst_mkapp();
inst_mkapp();
inst_push(2);
inst_push(5);
inst_mkapp();
inst_pushg(0);
inst_mkapp();
inst_mkapp();
inst_mkapp();
inst_eval();
inst_slide(2);
break;
default: fprintf(stderr, "Non-exhaustive pattern"); exit_program();
};
inst_slide(3);
}
void ff_sieve(void) {
inst_push(0);
inst_eval();
switch (mem(STACK_TOP)->tag) {
case 0:
inst_split();
inst_pushg(22);
inst_eval();
inst_slide(0);
break;
case 1:
inst_split();
inst_push(1);
inst_push(1);
inst_pushg(2);
inst_mkapp();
inst_pushg(3);
inst_mkapp();
inst_mkapp();
inst_pushg(4);
inst_mkapp();
inst_push(1);
inst_pushg(21);
inst_mkapp();
inst_mkapp();
inst_eval();
inst_slide(2);
break;
default: fprintf(stderr, "Non-exhaustive pattern"); exit_program();
};
inst_slide(2);
}
void ff_primes(void) {
inst_pushi(2);
inst_pushg(1);
inst_mkapp();
inst_pushg(4);
inst_mkapp();
inst_eval();
inst_update(0);
}
void ff_head(void) {
inst_push(0);
inst_eval();
switch (mem(STACK_TOP)->tag) {
case 0:
inst_split();
inst_pushi(0);
inst_slide(0);
break;
case 1:
inst_split();
inst_push(0);
inst_eval();
inst_slide(2);
break;
default: fprintf(stderr, "Non-exhaustive pattern"); exit_program();
};
inst_slide(2);
}
void ff_tail(void) {
inst_push(0);
inst_eval();
switch (mem(STACK_TOP)->tag) {
case 0:
inst_split();
inst_pushg(22);
inst_eval();
inst_slide(0);
break;
case 1:
inst_split();
inst_push(1);
inst_eval();
inst_slide(2);
break;
default: fprintf(stderr, "Non-exhaustive pattern"); exit_program();
};
inst_slide(2);
}
void ff_take(void) {
inst_push(1);
inst_pushg(7);
inst_mkapp();
inst_pushi(1);
inst_push(2);
inst_pushg(19);
inst_mkapp();
inst_mkapp();
inst_pushg(8);
inst_mkapp();
inst_mkapp();
inst_push(2);
inst_pushg(6);
inst_mkapp();
inst_pushg(21);
inst_mkapp();
inst_mkapp();
inst_pushg(22);
inst_push(2);
inst_pushi(0);
inst_pushg(15);
inst_mkapp();
inst_mkapp();
inst_pushg(0);
inst_mkapp();
inst_mkapp();
inst_mkapp();
inst_eval();
inst_slide(3);
}
void ff_main(void) {
inst_pushg(5);
inst_push(1);
inst_pushg(8);
inst_mkapp();
inst_mkapp();
inst_eval();
inst_slide(2);
}
void ff_not(void) {
inst_push(0);
inst_eval();
inst_not();
inst_slide(2);
}
void ff_or(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_or();
inst_slide(3);
}
void ff_and(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_and();
inst_slide(3);
}
void ff_5(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_isgt();
inst_slide(3);
}
void ff_6(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_islt();
inst_slide(3);
}
void ff_4(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_iseq();
inst_slide(3);
}
void ff_rem(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_rem();
inst_slide(3);
}
void ff_div(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_div();
inst_slide(3);
}
void ff_3(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_mul();
inst_slide(3);
}
void ff_2(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_sub();
inst_slide(3);
}
void ff_1(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_add();
inst_slide(3);
}
void ff_Cons(void) {
inst_push(1);
inst_push(1);
inst_pack(1,2);
inst_slide(3);
}
void ff_Nil(void) {
inst_pack(0,0);
inst_slide(1);
}
void ff_True(void) {
inst_pack(1,0);
inst_slide(1);
}
void ff_False(void) {
inst_pack(0,0);
inst_slide(1);
}
void global_init(void) {
addr_t ga;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 3;
mem(ga)->code = ff_ife;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 1;
mem(ga)->code = ff_from;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_notDivide;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_filter;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 1;
mem(ga)->code = ff_sieve;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 0;
mem(ga)->code = ff_primes;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 1;
mem(ga)->code = ff_head;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 1;
mem(ga)->code = ff_tail;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_take;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 1;
mem(ga)->code = ff_main;
main_func_addr = ga;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 1;
mem(ga)->code = ff_not;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_or;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_and;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_5;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_6;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_4;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_rem;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_div;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_3;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_2;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_1;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 2;
mem(ga)->code = ff_Cons;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 0;
mem(ga)->code = ff_Nil;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 0;
mem(ga)->code = ff_True;
ga = mem_alloc();
mem(ga)->type = NGlobal;
mem(ga)->g_arity = 0;
mem(ga)->code = ff_False;
}
