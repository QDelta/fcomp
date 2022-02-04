module GM.Compile
  ( CompiledCoreConstr,
    CompiledCoreFn,
    CompiledLiftedFn,
    CompiledCore,
    compile
  ) where

import Utils.Function
import Utils.Map
import Utils.Set
import Utils.State
import Common.Def
import Core.Def
import GM.Def
import Prim.Name
import Prim.GM

type CompiledCoreFn = (Name, Int, Code)          -- name, arity, code
type CompiledCoreConstr = (Name, Int, Int, Code) -- name, arity, tag, code
type CompiledLiftedFn = (Ident, Int, Code)       -- ident, arity, code

type CompiledCore =
  ( [CompiledCoreConstr]
  , [CompiledCoreFn]
  , [CompiledLiftedFn]
  )

data CompileEnv = CompileEnv
  { liftedFns :: [CompiledLiftedFn]
  , liftIndex :: Ident
  }

type CState a = State CompileEnv a

addLiftedFn :: Int -> Code -> CState Ident
addLiftedFn arity code = State $
  \env -> let id = liftIndex env in
    (id, env { liftedFns = (id, arity, code) : liftedFns env, liftIndex = id + 1 })

liftedApply :: Ident -> [Ident] -> CoreExpr
liftedApply lId params =
  foldl AppCE (LiftedFn lId) (map LVarCE params)

compileFn :: CoreFn -> CState CompiledCoreFn
compileFn (name, params, body) = do
  code <- compileWHNF (pushBinds emptyStack params) body
  return (name, arity, code ++ clean)
  where
    arity = length params
    clean = [Update arity, Pop arity]

-- eval expr to WHNF on stack
compileWHNF :: Stack -> CoreExpr -> CState Code
compileWHNF _ (IntCE n) =
  return [PushI n]
compileWHNF st (CaseCE e brs) = do
  eCode <- compileWHNF st e
  brCodes <- traverse (compileBranch st) brs
  return $ eCode ++ [Jump brCodes]
compileWHNF st e@(AppCE (GVarCE op) opr) =
  case mLookup (getIdent op) prim1 of
    Just inst -> do
      code <- compileWHNF st opr
      return $ code ++ [inst]
    Nothing -> do
      code <- compileLazy st e
      return $ code ++ [Eval]
compileWHNF st e@(AppCE (AppCE (GVarCE op) opr1) opr2) =
  case mLookup (getIdent op) prim2 of
    Just inst -> do
      code2 <- compileWHNF st opr2
      code1 <- compileWHNF (push st 1) opr1
      return $ code2 ++ code1 ++ [inst]
    Nothing -> do
      code <- compileLazy st e
      return $ code ++ [Eval]
compileWHNF st (LetCE (bindId, bindE) e) = do
  bCode <- compileLazy st bindE
  eCode <- compileWHNF (pushBinds st [bindId]) e
  return $ bCode ++ eCode ++ [Slide 1]
compileWHNF st (LetRecCE binds e) = do
  bCodes <- concat <$> traverse compLetR (zip bindEs [0..])
  eCode <- compileWHNF newStack e
  return $ Alloc n : bCodes ++ eCode ++ [Slide n]
  where
    n = length binds
    (bindIds, bindEs) = unzip binds
    newStack = pushBinds st bindIds
    compLetR (bindE, offset) = do
      code <- compileLazy newStack bindE
      return $ code ++ [Update offset]
compileWHNF st e = do
  code <- compileLazy st e
  return $ code ++ [Eval]

-- create a thunk on stack
compileLazy :: Stack -> CoreExpr -> CState Code
compileLazy _ (GVarCE name) =
  return [PushG name]
compileLazy _ (LiftedFn id) =
  return [PushL id]
compileLazy st (LVarCE i) =
  return [Push (getOffset st i)]
compileLazy _ (IntCE n) =
  return [PushI n]
compileLazy st (AppCE e1 e2) = do
  code2 <- compileLazy st e2
  code1 <- compileLazy (push st 1) e1
  return $ code2 ++ code1 ++ [MkApp]
compileLazy st e@(CaseCE _ _) = do
  (_, arity, code) <- compileFn (undefined, localPs, e)
  liftedId <- addLiftedFn arity code
  let lamExpr = liftedApply liftedId localPs
  compileLazy st lamExpr
  where
    localPs = sToList $ localVarsExcept emptySet e
compileLazy st (LetCE (bindId, bindE) e) = do
  bCode <- compileLazy st bindE
  eCode <- compileLazy (pushBinds st [bindId]) e
  return $ bCode ++ eCode ++ [Slide 1]
compileLazy st (LetRecCE binds e) = do
  bCodes <- concat <$> traverse compLetR (zip bindEs [0..])
  eCode <- compileLazy newStack e
  return $ Alloc n : bCodes ++ eCode ++ [Slide n]
  where
    n = length binds
    (bindIds, bindEs) = unzip binds
    newStack = pushBinds st bindIds
    compLetR (bindE, offset) = do
      code <- compileLazy newStack bindE
      return $ code ++ [Update offset]
compileLazy st (LambdaCE params e) = do
  (_, arity, code) <- compileFn (undefined, fullPs, e)
  liftedId <- addLiftedFn arity code
  let lamExpr = liftedApply liftedId localPs
  compileLazy st lamExpr
  where
    localPs = sToList $ localVarsExcept (sFromList params) e
    fullPs = localPs ++ params

compileBranch :: Stack -> CoreBranch -> CState (Int, Code)
compileBranch st (tag, binds, body) = do
  bCode <- compileWHNF (pushBinds st binds) body
  return (tag, Split : bCode ++ [Slide (length binds)])

compileConstr :: CoreConstr -> CompiledCoreConstr
compileConstr (name, arity, tag) =
  (name, arity, tag, pushP ++ [Pack tag arity, Update arity, Pop arity])
  where pushP = replicate arity (Push (arity - 1))

compile :: CoreProgram -> CompiledCore
compile (cs, fs) =
  ( map compileConstr cs
  , compiledPrimFns ++ compiledFns
  , liftedFns compEnv
  )
  where
    compFns = traverse compileFn fs
    (compiledFns, compEnv) = runState compFns initialCompileEnv

initialCompileEnv :: CompileEnv
initialCompileEnv = CompileEnv
  { liftedFns = []
  , liftIndex = 0
  }

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
