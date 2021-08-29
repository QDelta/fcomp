module CodeGen.CGen where

import Utils
import GM.Compile
import GM.Def
import GM.Heap

type CCode = String

codeGen :: CompiledCore -> CCode
codeGen p =
  concatMap (genFn h m) ml ++
  genInitHeap h ml m
  where 
    (_, _, _, h, m) = initialState p
    ml = mToList m

mangle :: String -> String
mangle n = case n of
  "+" -> "ff_1"
  "-" -> "ff_2"
  "*" -> "ff_3"
  _   -> "ff_" ++ n

genFn :: Heap Node -> GlobalMap -> (String, Addr) -> CCode
genFn h m (name, a) =
  "void " ++ mangle name ++ "(void) {\n" ++
  genCode m code ++
  "}\n"
  where (NGlobal _ code) = hLookup h a

genInitHeap :: Heap Node -> [(String, Addr)] -> GlobalMap -> CCode
genInitHeap h ml m = 
  "void heap_init(void) {\n" ++
  "init_heap = malloc(sizeof(node_t) * " ++ show (mCount m) ++ ");\n" ++
  "main_func_id = " ++ show (mLookup m "main") ++ ";\n" ++
  concatMap (assignInitHeap h) ml ++ "}\n"

assignInitHeap :: Heap Node -> (String, Addr) -> CCode
assignInitHeap h (name, a) =
  "init_heap[" ++ show a ++ "].type = NGlobal;\n" ++
  "init_heap[" ++ show a ++ "].g_arity = " ++ show arity ++ ";\n" ++
  "init_heap[" ++ show a ++ "].code = " ++ mangle name ++ ";\n"
  where (NGlobal arity _) = hLookup h a

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
