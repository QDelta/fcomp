module GM.Compile
  ( CompiledCoreConstr,
    CompiledCoreFn,
    CompiledCore,
    compile
  ) where

import Utils.Function
import Utils.Map
import Common.Def
import Core.Def
import GM.Def
import Prim.Name
import Prim.GM

type CompiledCoreFn = (Name, Int, Code)          -- name, arity, code
type CompiledCoreConstr = (Name, Int, Int, Code) -- name, arity, tag, code
type CompiledCore = ([CompiledCoreConstr], [CompiledCoreFn])

type Stack = [(Int, Map Ident Int)] -- nested maps for stack offset

-- params are pushed from right to left
-- bindings on stack: (top) $5, $6, $2, $3, $4, $0, $1 (bottom)

getOffset :: Stack -> Ident -> Int
getOffset [] _ = undefined
getOffset ((o, m) : rest) id =
  case mLookup id m of
    Just i -> i
    Nothing -> o + getOffset rest id

pushBinds :: Stack -> [Ident] -> Stack
pushBinds st ids =
  (length ids, mFromList $ zip ids [0..]) : st

push :: Stack -> Int -> Stack
push st n = (n, emptyMap) : st

emptyStack :: Stack
emptyStack = []

compileFn :: CoreFn -> CompiledCoreFn
compileFn (name, params, body) =
  (name, arity, code ++ clean)
  where
    arity = length params
    code = compileWHNF (pushBinds emptyStack params) body
    clean = [Update arity, Pop arity]

-- eval expr to WHNF on stack
compileWHNF :: Stack -> CoreExpr -> Code
compileWHNF _ (IntCE n) =
  [PushI n]
compileWHNF st (CaseCE e brs) =
  compileWHNF st e ++ [Jump (compileBranches st brs)]
compileWHNF st e@(AppCE (GVarCE op) opr) =
  case mLookup (getIdent op) prim1 of
    Just inst -> compileWHNF st opr ++ [inst]
    Nothing -> compileLazy st e ++ [Eval]
compileWHNF st e@(AppCE (AppCE (GVarCE op) opr1) opr2) =
  case mLookup (getIdent op) prim2 of
    Just inst -> compileWHNF st opr2 ++ compileWHNF (push st 1) opr1 ++ [inst]
    Nothing -> compileLazy st e ++ [Eval]
compileWHNF st (LetCE (bindId, bindE) e) =
  compileLazy st bindE ++ compileWHNF (pushBinds st [bindId]) e ++ [Slide 1]
compileWHNF st (LetRecCE binds e) =
  Alloc n :
       concatMap compLetR (zip bindEs [0..])
    ++ compileWHNF newStack e ++ [Slide n]
  where
    n = length binds
    (bindIds, bindEs) = unzip binds
    newStack = pushBinds st bindIds
    compLetR (bindE, offset) = compileLazy newStack bindE ++ [Update offset]
compileWHNF f e = compileLazy f e ++ [Eval]

-- create a thunk on stack
-- TODO: lazy case: generate a function (lambda lifting)
compileLazy :: Stack -> CoreExpr -> Code
compileLazy _ (GVarCE name) =
  [PushG name]
compileLazy st (LVarCE i) =
  [Push (getOffset st i)]
compileLazy _ (IntCE n) = [PushI n]
compileLazy st (AppCE e1 e2) =
  compileLazy st e2 ++ compileLazy (push st 1) e1 ++ [MkApp]
compileLazy st (CaseCE e brs) =
  error "case expression in lazy environment are not implemented yet, use a function to wrap it."
compileLazy st (LetCE (bindId, bindE) e) =
  compileLazy st bindE ++ compileLazy (pushBinds st [bindId]) e ++ [Slide 1]
compileLazy st (LetRecCE binds e) =
  Alloc n :
       concatMap compLetR (zip bindEs [0..])
    ++ compileLazy newStack e ++ [Slide n]
  where
    n = length binds
    (bindIds, bindEs) = unzip binds
    newStack = pushBinds st bindIds
    compLetR (bindE, offset) = compileLazy newStack bindE ++ [Update offset]

compileBranches :: Stack -> [CoreBranch] -> [(Int, Code)]
compileBranches st = map (compileBranch st)

compileBranch :: Stack -> CoreBranch -> (Int, Code)
compileBranch st (tag, binds, body) =
  (tag, code)
  where
    code = Split : compileWHNF newStack body ++ [Slide (length binds)]
    newStack = pushBinds st binds

compileConstr :: CoreConstr -> CompiledCoreConstr
compileConstr (name, arity, tag) =
  (name, arity, tag, pushP ++ [Pack tag arity, Update arity, Pop arity])
  where pushP = replicate arity (Push (arity - 1))

compile :: CoreProgram -> CompiledCore
compile (cs, fs) =
  (map compileConstr cs, compiledPrimFns ++ map compileFn fs)

prim1 :: Map Ident Instruction
prim1 =
  foldl
  (\m (s, inst) ->
    mInsert (getIdent (primNames ! s), inst) m)
  emptyMap
  prim1Insts

prim2 :: Map Ident Instruction
prim2 =
  foldl
  (\m (s, inst) ->
    mInsert (getIdent (primNames ! s), inst) m)
  emptyMap
  prim2Insts
