module GM.Compile where

import Utils
import Type.Core
import GM.Def
import GM.Heap

type Frame = ([Int], Int)
-- bindings in stack: (top) $5, $6, $2, $3, $4, $0, $1 (bottom)
-- => fst Frame = [5, 2]
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
popStack ([], _) = ([], 0) -- dummy
popStack (min : rest, count) = (rest, newCount)
  where newCount = min - getTop rest

getOffset :: Frame -> Int -> Int
getOffset f@(ms, count) n =
  if n >= min then n - min else count + nextOffset
  where 
    min = getTop ms
    nextOffset = getOffset (popStack f) n

strictBinOpList :: [(String, Instruction)]
strictBinOpList =
  [ ("+", Add),
    ("-", Sub),
    ("*", Mul)
  ]

strictBinOps :: Map String Instruction
strictBinOps = mFromList strictBinOpList

compileStrictBinOp :: (String, Instruction) -> CompiledCoreFn
compileStrictBinOp (name, inst) =
  (name, 2, [Push 1, Eval, Push 1, Eval, inst, Update 2, Pop 2])

compiledBinOps :: [CompiledCoreFn]
compiledBinOps = map compileStrictBinOp strictBinOpList

type CompiledCoreFn = (String, Int, Code)

compileFn :: CoreFn -> CompiledCoreFn
compileFn (n, a, b) = (n, a, code ++ clean)
  where
    code = compileWHNF (initialFrame a) b
    clean = 
      if a == 0  -- CAF
      then [Update 0]
      else [Slide (a + 1)]

-- Simple strict analysis
compileWHNF :: Frame -> CoreExpr -> Code
compileWHNF _ (IntCE n) = [PushI n]
compileWHNF f (CaseCE e brs) = compileWHNF f e ++ [Jump (compileBranches f brs)]
compileWHNF f (AppCE (AppCE (GVarCE op) e1) e2) | op `mElem` strictBinOps =
  compileWHNF f e2 ++ compileWHNF (pushStack f 1) e1 ++ [mLookup strictBinOps op]
compileWHNF f e = compileLazy f e ++ [Eval]

-- TODO: lazy case
compileLazy :: Frame -> CoreExpr -> Code
compileLazy _ (GVarCE name) = [PushG name]
compileLazy f (LVarCE i) = [Push (getOffset f i)]
compileLazy _ (IntCE n) = [PushI n]
compileLazy f (AppCE e1 e2) = 
  compileLazy f e2 ++ compileLazy (pushStack f 1) e1 ++ [MkApp]
compileLazy f (CaseCE e brs) = error "lazy case expressions are not implemented yet, use a function to wrap it."

compileBranches :: Frame -> [CoreBranch] -> Map Int Code
compileBranches f brs = mFromList (map (compileBranch f) brs)

compileBranch :: Frame -> CoreBranch -> (Int, Code)
compileBranch f (a, t, b) = (t, code)
  where
    code = Split : compileWHNF newF b ++ [Slide a]
    newF = pushStack f a

type CompiledCoreConstr = (String, Int, Int, Code)

compileConstr :: CoreConstr -> CompiledCoreConstr
compileConstr (name, arity, tag) = 
  (name, arity, tag, pushP ++ [Pack tag arity, Slide (arity + 1)])
  where pushP = replicate arity (Push (arity - 1))

compiledPrimFn :: [CompiledCoreFn] -- name, arity, code
compiledPrimFn = compiledBinOps

type CompiledCore = ([CompiledCoreConstr], [CompiledCoreFn])

compile :: CoreProgram -> CompiledCore
compile (cs, fs) = (map compileConstr cs, compiledPrimFn ++ map compileFn fs)

initialState :: CompiledCore -> State
initialState (cs, fs) = ([PushG "main", Eval], [], [], initialHeap, initialGlobalM)
  where
    (heap1, gm1) = foldl allocConstr (emptyHeap, emptyMap) cs
    (initialHeap, initialGlobalM) = foldl allocFn (heap1, gm1) fs

allocConstr :: (Heap Node, GlobalMap) -> CompiledCoreConstr -> (Heap Node, GlobalMap)
allocConstr (h, m) cons@(n, a, t, c) = (newH, newM)
  where
    (newH, addr) = hAlloc h (NGlobal a c)
    newM = mInsert m (n, addr)

allocFn :: (Heap Node, GlobalMap) -> CompiledCoreFn -> (Heap Node, GlobalMap)
allocFn (h, m) f@(n, a, c) = (newH, newM)
  where
    (newH, addr) = hAlloc h (NGlobal a c)
    newM = mInsert m (n, addr)
