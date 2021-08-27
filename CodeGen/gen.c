#include "runtime.h"
node_t init_heap[7];
#include "runtime.c"
void func6(void) {
inst_push(2);
inst_push(2);
inst_mkapp();
inst_push(3);
inst_push(2);
inst_mkapp();
inst_mkapp();
inst_eval();
inst_slide(4);
inst_unwind();
}
void func5(void) {
inst_push(0);
inst_eval();
inst_slide(3);
inst_unwind();
}
void func4(void) {
inst_pushg(5);
inst_pushg(5);
inst_pushg(6);
inst_mkapp();
inst_mkapp();
inst_eval();
inst_update(0);
inst_unwind();
}
void func3(void) {
inst_pushi(3);
inst_pushg(4);
inst_mkapp();
inst_eval();
inst_pushi(2);
inst_pushg(4);
inst_mkapp();
inst_eval();
inst_pushi(1);
inst_pushg(4);
inst_mkapp();
inst_eval();
inst_add();
inst_add();
inst_update(0);
inst_unwind();
}
void func2(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_mul();
inst_update(2);
inst_pop(2);
inst_unwind();
}
void func1(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_sub();
inst_update(2);
inst_pop(2);
inst_unwind();
}
void func0(void) {
inst_push(1);
inst_eval();
inst_push(1);
inst_eval();
inst_add();
inst_update(2);
inst_pop(2);
inst_unwind();
}
void heap_init(void) {
init_heap[6].type = NGlobal;
init_heap[6].g_arity = 3;
init_heap[6].code = func6;
init_heap[5].type = NGlobal;
init_heap[5].g_arity = 2;
init_heap[5].code = func5;
init_heap[4].type = NGlobal;
init_heap[4].g_arity = 0;
init_heap[4].code = func4;
init_heap[3].type = NGlobal;
init_heap[3].g_arity = 0;
init_heap[3].code = func3;
init_heap[2].type = NGlobal;
init_heap[2].g_arity = 2;
init_heap[2].code = func2;
init_heap[1].type = NGlobal;
init_heap[1].g_arity = 2;
init_heap[1].code = func1;
init_heap[0].type = NGlobal;
init_heap[0].g_arity = 2;
init_heap[0].code = func0;
}
int main(void) {
heap_init();
stack_init();
inst_pushg(3);
inst_eval();
switch (STACK_TOP->type) {
case NInt:
printf("%d\n", STACK_TOP->intv);
default: break;}
return 0;
}