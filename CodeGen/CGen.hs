module CodeGen.CGen where

import Utils
import GM.Compile
import GM.Def
import GM.Heap

type CCode = String

codeGen :: CompiledCore -> CCode
codeGen p =
  concatMap (genFn newM) nnl ++
  initG
  where 
    (_, _, _, h, m) = initialState p
    nnl = genNN (mToList m) h
    (initG, newM) = genInitGlobal nnl

genNN :: [(String, Addr)] -> Heap Node -> [(String, Node)]
genNN m h = map (second (hLookup h)) m

-- TODO: a better mangle function
mangle :: String -> String
mangle n = case n of
  "+" -> "ff_1"
  "-" -> "ff_2"
  "*" -> "ff_3"
  "=?" -> "ff_4"
  ">?" -> "ff_5"
  "<?" -> "ff_6"
  _   -> "ff_" ++ n

genFn :: GlobalMap -> (String, Node) -> CCode
genFn m (name, NGlobal _ code) =
  "void " ++ mangle name ++ "(void) {\n" ++
  genCode m code ++
  "}\n"

genInitGlobal :: [(String, Node)] -> (CCode, GlobalMap)
genInitGlobal nnl = (ccode, m)
  where
    ccode =
      "void global_init(void) {\n" ++
      "addr_t ga;\n" ++
      concatMap allocInitGlobal nnl ++ "}\n"
    m = mFromList (zip (map fst nnl) [0..])

allocInitGlobal :: (String, Node) -> CCode
allocInitGlobal (name, NGlobal arity _) =
  "ga = mem_alloc();\n" ++
  "mem(ga)->type = NGlobal;\n" ++
  "mem(ga)->g_arity = " ++ show arity ++ ";\n" ++
  "mem(ga)->code = " ++ mangle name ++ ";\n" ++
  assignMainAddr
    where
      assignMainAddr = if name == "main" then "main_func_addr = ga;\n" else ""

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
genInstr m Div =
  "inst_div();\n"
genInstr m Rem =
  "inst_rem();\n"
genInstr m IsEq =
  "inst_iseq();\n"
genInstr m IsGt =
  "inst_isgt();\n"
genInstr m IsLt =
  "inst_islt();\n"
genInstr m And =
  "inst_and();\n"
genInstr m Or =
  "inst_or();\n"
genInstr m Not =
  "inst_not();\n"
genInstr m (Jump brs) =
  "switch (mem(STACK_TOP)->tag) {\n" ++
  concatMap (genCase m) (mToList brs) ++
  "default: fprintf(stderr, \"Non-exhaustive pattern\"); exit_program();\n" ++
  "};\n"

genCase :: GlobalMap -> (Int, [Instruction]) -> CCode
genCase m (tag, code) =
  "case " ++ show tag ++ ":\n" ++
  genCode m code ++ "break;\n"
