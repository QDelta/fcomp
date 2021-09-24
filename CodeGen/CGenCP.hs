module CodeGen.CGenCP (codeGen) where

import Utils.Map
import GM.Compile
import GM.Def

type CCode = String

initGlobals :: CompiledCore -> [(String, Node)]
initGlobals (cs, fs) = map initConstr cs ++ map initFn fs

initConstr :: CompiledCoreConstr -> (String, Node)
initConstr (n, a, _, c) = (n, NGlobal a c)

initFn :: CompiledCoreFn -> (String, Node)
initFn (n, a, c) = (n, NGlobal a c)

codeGen :: CompiledCore -> CCode
codeGen p =
  concatMap genFnGOffset nnl ++
  concatMap genFn nnl ++
  initG
  where
    nnl = initGlobals p
    initG = genInitGlobal nnl

mangle :: String -> String
mangle = ("f_" ++)

mangleGOffset :: String -> String
mangleGOffset name = mangle name ++ "_offset"

genFnGOffset :: (String, Node) -> CCode
genFnGOffset (name, _) = "int64_t " ++ mangleGOffset name ++ ";\n"

genFn :: (String, Node) -> CCode
genFn (name, NGlobal _ code) =
  "void " ++ mangle name ++ "(void) {\n" ++
  genCode code ++
  "}\n"

genInitGlobal :: [(String, Node)] -> CCode
genInitGlobal nnl = 
  -- "node_t global_funcs[" ++ show (length nnl) ++ "];\n" ++
  "void global_init(void) {\n" ++
  "global_func_num = " ++ show (length nnl) ++ ";\n" ++
  "global_funcs = malloc(global_func_num * sizeof(node_t));\n" ++
  concatMap allocInitGlobal (zip [0..] nnl) ++ "}\n"

allocInitGlobal :: (Int, (String, Node)) -> CCode
allocInitGlobal (offset, (name, NGlobal arity _)) =
  fNode ++ ".type = NGLOBAL;\n" ++
  fNode ++ ".g_arity = " ++ show arity ++ ";\n" ++
  fNode ++ ".code = " ++ mangle name ++ ";\n" ++
  fNode ++ ".gc_copied = TRUE;\n" ++
  fNode ++ ".gc_forwarding = " ++ fNodePtr ++ ";\n" ++
  fNode ++ ".gc_isglobal = TRUE;\n" ++
  mangleGOffset name ++ " = " ++ offsetStr ++ ";\n" ++
  assignStartAddr
  where
    offsetStr = show offset
    fNode = "global_funcs[" ++ offsetStr ++ "]"
    fNodePtr = "(global_funcs + " ++ offsetStr ++ ")"
    assignStartAddr = 
      if name == "start" then "entry_func_offset = " ++ offsetStr ++ ";\n" else ""

genCode :: Code -> CCode
genCode = concatMap genInstr

genInstr :: Instruction -> CCode
genInstr (PushG name) = 
  "inst_pushg(" ++ mangleGOffset name ++ ");\n"
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
  "switch (STACK_TOP->tag) {\n" ++
  concatMap genCase brs ++
  "default: fprintf(stderr, \"Non-exhaustive pattern\"); exit_program();\n" ++
  "};\n"

genCase :: (Int, [Instruction]) -> CCode
genCase (tag, code) =
  "case " ++ show tag ++ ":\n" ++
  genCode code ++ "break;\n"
