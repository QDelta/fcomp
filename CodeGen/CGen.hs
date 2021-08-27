module CodeGen.CGen where

import Utils
import GM.Compile
import GM.Def
import GM.Heap

type CCode = String

codeGen :: CompiledCore -> CCode
codeGen p =
  "#include \"runtime.h\"\n" ++
  genInitHeap (mCount m) ++
  "#include \"runtime.c\"\n" ++
  concatMap (genFn m) gList ++
  "void heap_init(void) {\n" ++
  concatMap assignInitHeap gList ++ "}\n" ++
  genMain m
  where 
    (_, _, _, h, m) = initialState p
    gList = hToList h

mangle :: Addr -> String
mangle i = "func" ++ show i

genFn :: GlobalMap -> (Addr, Node) -> CCode
genFn m (i, NGlobal arity code) =
  "void " ++ mangle i ++ "(void) {\n" ++
  genCode m code ++
  "}\n"

genInitHeap :: Int -> CCode
genInitHeap n = "node_t init_heap[" ++ show n ++ "];\n"

assignInitHeap :: (Addr, Node) -> CCode
assignInitHeap (i, NGlobal arity code) =
  "init_heap[" ++ show i ++ "].type = NGlobal;\n" ++
  "init_heap[" ++ show i ++ "].g_arity = " ++ show arity ++ ";\n" ++
  "init_heap[" ++ show i ++ "].code = " ++ mangle i ++ ";\n"

genCode :: GlobalMap -> Code -> CCode
genCode m = concatMap (genInstr m)

genInstr :: GlobalMap -> Instruction -> CCode
genInstr m (PushG name) = 
  "inst_pushg(" ++ show (mLookup m name) ++ ");\n"
genInstr m (PushI n) =
  "inst_pushi(" ++ show n ++ ");\n"
genInstr m (Push o) =
  "inst_push(" ++ show o ++ ");\n"
genInstr m (Pop n) =
  "inst_pop(" ++ show n ++ ");\n"
genInstr m MkApp =
  "inst_mkapp();\n"
genInstr m (Update o) =
  "inst_update(" ++ show o ++ ");\n"
genInstr m (Pack t a) =
  "inst_pack(" ++ show t ++ "," ++ show a ++ ");\n"
genInstr m Split =
  "inst_split();\n"
genInstr m (Slide n) =
  "inst_slide(" ++ show n ++ ");\n"
genInstr m Eval =
  "inst_eval();\n"
genInstr m Unwind =
  "inst_unwind();\n"
genInstr m (Alloc n) =
  "inst_alloc(" ++ show n ++ ");\n"
genInstr m Add =
  "inst_add();\n"
genInstr m Sub =
  "inst_sub();\n"
genInstr m Mul =
  "inst_mul();\n"
genInstr m (Jump brs) =
  "switch (STACK_TOP->tag) {\n" ++
  concatMap (genCase m) (mToList brs) ++
  "default: abort();\n" ++
  "};"

genCase :: GlobalMap -> (Int, [Instruction]) -> CCode
genCase m (tag, code) =
  "case " ++ show tag ++ ":\n" ++
  genCode m code ++ "break;\n"

genMain :: GlobalMap -> CCode
genMain m =
  "int main(void) {\n" ++
  "heap_init();\n" ++
  "stack_init();\n" ++
  genInstr m (PushG "main") ++
  genInstr m Eval ++
  printResult ++
  "return 0;\n}"

printResult :: CCode
printResult =
  "switch (STACK_TOP->type) {\n" ++
  "case NInt:\n" ++
  "printf(\"%d\\n\", STACK_TOP->intv);\n" ++
  "default: break;}\n"
