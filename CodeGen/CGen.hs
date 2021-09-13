module CodeGen.CGen where

import Utils.Map
import GM.Compile
import GM.Def

type CCode = String

codeGen :: CompiledCore -> CCode
codeGen p =
  concatMap genFnAddr nnl ++
  concatMap genFn nnl ++
  initG
  where
    nnl = initGlobals p
    initG = genInitGlobal nnl

mangle :: String -> String
mangle = ("f_" ++)

mangleAddr :: String -> String
mangleAddr name = mangle name ++ "_addr"

genFnAddr :: (String, Node) -> CCode
genFnAddr (name, _) = "addr_t " ++ mangleAddr name ++ ";\n"

genFn :: (String, Node) -> CCode
genFn (name, NGlobal _ code) =
  "void " ++ mangle name ++ "(void) {\n" ++
  genCode code ++
  "}\n"

genInitGlobal :: [(String, Node)] -> CCode
genInitGlobal nnl = 
  "void global_init(void) {\n" ++
  "addr_t ga;\n" ++
  concatMap allocInitGlobal nnl ++ "}\n"

allocInitGlobal :: (String, Node) -> CCode
allocInitGlobal (name, NGlobal arity _) =
  "ga = mem_alloc();\n" ++
  "mem(ga)->type = NGlobal;\n" ++
  "mem(ga)->g_arity = " ++ show arity ++ ";\n" ++
  "mem(ga)->code = " ++ mangle name ++ ";\n" ++
  mangleAddr name ++ " = ga;\n" ++
  assignStartAddr
  where
    assignStartAddr = if name == "start" then "entry_func_addr = ga;\n" else ""

genCode :: Code -> CCode
genCode = concatMap genInstr

genInstr :: Instruction -> CCode
genInstr (PushG name) = 
  "inst_pushg(" ++ mangleAddr name ++ ");\n"
genInstr (PushI n) =
  "inst_pushi(" ++ show n ++ ");\n"
genInstr (Push o) =
  "inst_push(" ++ show o ++ ");\n"
genInstr (Pop n) =
  "inst_pop(" ++ show n ++ ");\n"
genInstr MkApp =
  "inst_mkapp();\n"
genInstr (Update o) =
  "inst_update(" ++ show o ++ ");\n"
genInstr (Pack t a) =
  "inst_pack(" ++ show t ++ "," ++ show a ++ ");\n"
genInstr Split =
  "inst_split();\n"
genInstr (Slide n) =
  "inst_slide(" ++ show n ++ ");\n"
genInstr Eval =
  "inst_eval();\n"
genInstr Unwind =
  "inst_unwind();\n"
genInstr (Alloc n) =
  "inst_alloc(" ++ show n ++ ");\n"
genInstr Add =
  "inst_add();\n"
genInstr Sub =
  "inst_sub();\n"
genInstr Mul =
  "inst_mul();\n"
genInstr Div =
  "inst_div();\n"
genInstr Rem =
  "inst_rem();\n"
genInstr IsEq =
  "inst_iseq();\n"
genInstr IsGt =
  "inst_isgt();\n"
genInstr IsLt =
  "inst_islt();\n"
genInstr IsNe =
  "inst_isne();\n"
genInstr IsGe =
  "inst_isge();\n"
genInstr IsLe =
  "inst_isle();\n"
genInstr Not =
  "inst_not();\n"
genInstr (Jump brs) =
  "switch (mem(STACK_TOP)->tag) {\n" ++
  concatMap genCase brs ++
  "default: fprintf(stderr, \"Non-exhaustive pattern\"); exit_program();\n" ++
  "};\n"

genCase :: (Int, [Instruction]) -> CCode
genCase (tag, code) =
  "case " ++ show tag ++ ":\n" ++
  genCode code ++ "break;\n"
