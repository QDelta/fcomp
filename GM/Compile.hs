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

type Frame = ([Int], Int)
-- bindings on stack: (top) $5, $6, $2, $3, $4, $0, $1 (bottom)
-- => fst Frame = [5, 2], represents $5, .., $2, .., $0, ..
--    snd Frame = length [$5, $6] = 2

getTop :: [Int] -> Int
getTop [] = 0
getTop (m : _) = m

initialFrame :: Int -> Frame
initialFrame n = ([], n)

pushStack :: Frame -> Int -> Frame
pushStack (ms, count) n = (newMin : ms, n)
  where newMin = count + getTop ms

popStack :: Frame -> Frame
popStack ([], _) = ([], 0)
popStack (min : rest, count) = (rest, newCount)
  where newCount = min - getTop rest

getOffset :: Frame -> Int -> Int
getOffset f@(ms, count) n =
  if n >= min then n - min else count + nextOffset
  where
    min = getTop ms
    nextOffset = getOffset (popStack f) n

compileFn :: CoreFn -> CompiledCoreFn
compileFn (n, a, b) = (n, a, code ++ clean)
  where
    code = compileWHNF (initialFrame a) b
    clean = [Update a, Pop a]

-- eval expr to WHNF on stack
compileWHNF :: Frame -> CoreExpr -> Code
compileWHNF _ (IntCE n) = [PushI n]
compileWHNF f (CaseCE e brs) = compileWHNF f e ++ [Jump (compileBranches f brs)]
compileWHNF f e@(AppCE (GVarCE op) opr) =
  case mLookup (getIdent op) prim1 of
    Just inst -> compileWHNF f opr ++ [inst]
    Nothing -> compileLazy f e ++ [Eval]
compileWHNF f e@(AppCE (AppCE (GVarCE op) opr1) opr2) =
  case mLookup (getIdent op) prim2 of
    Just inst -> compileWHNF f opr2 ++ compileWHNF (pushStack f 1) opr1 ++ [inst]
    Nothing -> compileLazy f e ++ [Eval]
compileWHNF f (LetCE bindE e) =
  compileLazy f bindE ++ compileWHNF (pushStack f 1) e ++ [Slide 1]
compileWHNF f (LetRecCE bindEs e) =
  Alloc n :
       concatMap compLetR (zip bindEs [1..])
    ++ compileWHNF newF e ++ [Slide n]
  where
    n = length bindEs
    newF = pushStack f n
    compLetR (bindE, offset) = compileLazy newF bindE ++ [Update (n - offset)]
compileWHNF f e = compileLazy f e ++ [Eval]

-- create a thunk on stack
-- TODO: lazy case: generate a function (lambda lifting)
compileLazy :: Frame -> CoreExpr -> Code
compileLazy _ (GVarCE name) = [PushG name]
compileLazy f (LVarCE i) = [Push (getOffset f i)]
compileLazy _ (IntCE n) = [PushI n]
compileLazy f (AppCE e1 e2) =
  compileLazy f e2 ++ compileLazy (pushStack f 1) e1 ++ [MkApp]
compileLazy f (CaseCE e brs) =
  error "case expression in lazy environment are not implemented yet, use a function to wrap it."
compileLazy f (LetCE bindE e) =
  compileLazy f bindE ++ compileLazy (pushStack f 1) e ++ [Slide 1]
compileLazy f (LetRecCE bindEs e) =
  Alloc n :
       concatMap compLetR (zip bindEs [1..])
    ++ compileLazy newF e ++ [Slide n]
  where
    n = length bindEs
    newF = pushStack f n
    compLetR (bindE, offset) = compileLazy newF bindE ++ [Update (n - offset)]

compileBranches :: Frame -> [CoreBranch] -> [(Int, Code)]
compileBranches f = map (compileBranch f)

compileBranch :: Frame -> CoreBranch -> (Int, Code)
compileBranch f (a, t, b) = (t, code)
  where
    code = Split : compileWHNF newF b ++ [Slide a]
    newF = pushStack f a

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
