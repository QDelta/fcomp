module GM.Compile where

import Utils.Map
import Type.CoreDef
import GM.Def

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

strictFnList :: [(String, Int, Instruction)]
strictFnList = 
  [ ("add", 2, Add ),
    ("sub", 2, Sub ),
    ("mul", 2, Mul ),
    ("div", 2, Div ),
    ("rem", 2, Rem ),
    ("eq",  2, IsEq),
    ("lt",  2, IsLt),
    ("gt",  2, IsGt),
    ("ne",  2, IsNe),
    ("le",  2, IsLe),
    ("ge",  2, IsGe),
    ("not", 1, Not )
  ]

filterArity :: Int -> [(String, Int, Instruction)] -> Map String Instruction
filterArity n =
  mFromList . map (\(a, b, c) -> (a, c)) . filter (\(a, b, c) -> b == n)

-- TODO: a general matcher for strict ops
strictBinFns :: Map String Instruction
strictBinFns = filterArity 2 strictFnList

strictUnaryFns :: Map String Instruction
strictUnaryFns = filterArity 1 strictFnList

compileStrictFn :: (String, Int, Instruction) -> CompiledCoreFn
compileStrictFn (name, arity, inst) =
  (name, arity, concat (replicate arity [Push (arity - 1), Eval]) ++ [inst, Update arity, Pop arity])

compiledStrictFns :: [CompiledCoreFn]
compiledStrictFns = map compileStrictFn strictFnList

type CompiledCoreFn = (String, Int, Code)

compileFn :: CoreFn -> CompiledCoreFn
compileFn (n, a, b) = (n, a, code ++ clean)
  where
    code = compileWHNF (initialFrame a) b
    clean = [Update a, Pop a]

-- Simple strictness analysis
compileWHNF :: Frame -> CoreExpr -> Code
compileWHNF _ (IntCE n) = [PushI n]
compileWHNF f (CaseCE e brs) = compileWHNF f e ++ [Jump (compileBranches f brs)]
compileWHNF f (AppCE (GVarCE op) e) | op `mElem` strictUnaryFns =
  compileWHNF f e ++ [mLookup strictUnaryFns op (error "")]
compileWHNF f (AppCE (AppCE (GVarCE op) e1) e2) | op `mElem` strictBinFns =
  compileWHNF f e2 ++ compileWHNF (pushStack f 1) e1 ++ [mLookup strictBinFns op (error "")]
compileWHNF f e = compileLazy f e ++ [Eval]

-- TODO: lazy case: generate a function
compileLazy :: Frame -> CoreExpr -> Code
compileLazy _ (GVarCE name) = [PushG name]
compileLazy f (LVarCE i) = [Push (getOffset f i)]
compileLazy _ (IntCE n) = [PushI n]
compileLazy f (AppCE e1 e2) = 
  compileLazy f e2 ++ compileLazy (pushStack f 1) e1 ++ [MkApp]
compileLazy f (CaseCE e brs) = 
  error "case expression in lazy environment are not implemented yet, use a function to wrap it."

compileBranches :: Frame -> [CoreBranch] -> [(Int, Code)]
compileBranches f = map (compileBranch f)

compileBranch :: Frame -> CoreBranch -> (Int, Code)
compileBranch f (a, t, b) = (t, code)
  where
    code = Split : compileWHNF newF b ++ [Slide a]
    newF = pushStack f a

type CompiledCoreConstr = (String, Int, Int, Code)

compileConstr :: CoreConstr -> CompiledCoreConstr
compileConstr (name, arity, tag) = 
  (name, arity, tag, pushP ++ [Pack tag arity, Update arity, Pop arity])
  where pushP = replicate arity (Push (arity - 1))

compiledPrimFn :: [CompiledCoreFn] -- name, arity, code
compiledPrimFn = compiledStrictFns ++
  [ ("and", 2, [Push 0, Eval, Jump [(0, [Pop 1, Pack 0 0]), (1, [Pop 1, Push 1, Eval])], Update 2, Pop 2]),
    ("or",  2, [Push 0, Eval, Jump [(0, [Pop 1, Push 1, Eval]), (1, [Pop 1, Pack 1 0])], Update 2, Pop 2])
  ]
-- (def (and x y) (case x (False False) (True  y)))
-- (def (or  x y) (case x (True  True ) (False y)))

type CompiledCore = ([CompiledCoreConstr], [CompiledCoreFn])

compile :: CoreProgram -> CompiledCore
compile (cs, fs) = (map compileConstr cs, compiledPrimFn ++ map compileFn fs)

initGlobals :: CompiledCore -> [(String, Node)]
initGlobals (cs, fs) = map initConstr cs ++ map initFn fs

initConstr :: CompiledCoreConstr -> (String, Node)
initConstr (n, a, _, c) = (n, NGlobal a c)

initFn :: CompiledCoreFn -> (String, Node)
initFn (n, a, c) = (n, NGlobal a c)
