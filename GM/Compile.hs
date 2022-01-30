module GM.Compile
  ( CompiledCoreConstr,
    CompiledCoreFn,
    CompiledCore,
    compile
  ) where

import Utils.Function
import Utils.Map
import Core.Def
import GM.Def

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

primInt2 :: [(String, Instruction)]
primInt2 =
  [ ("add", Add),
    ("sub", Sub),
    ("mul", Mul),
    ("div", Div),
    ("rem", Rem)
  ]

primBool2 :: [(String, Instruction)]
primBool2 =
  [ ("eq", IsEq),
    ("lt", IsLt),
    ("gt", IsGt),
    ("ne", IsNe),
    ("le", IsLe),
    ("ge", IsGe)
  ]

primBool1 :: [(String, Instruction)]
primBool1 =
  [ ("not", Not)
  ]

prim2 :: [(String, Instruction)]
prim2 = primInt2 ++ primBool2

prim1 :: [(String, Instruction)]
prim1 = primBool1

compilePrimFn :: Int -> (String, Instruction) -> CompiledCoreFn
compilePrimFn arity (name, inst) =
  (name, arity, concat (replicate arity [Push (arity - 1), Eval]) ++ [inst, Update arity, Pop arity])

compiledPrimFns :: [CompiledCoreFn]
compiledPrimFns =
  map (compilePrimFn 2) prim2 ++ map (compilePrimFn 1) prim1

type CompiledCoreFn = (String, Int, Code) -- name, arity, code

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
  case lookup op prim1 of
    Just inst -> compileWHNF f opr ++ [inst]
    Nothing -> compileLazy f e ++ [Eval]
compileWHNF f e@(AppCE (AppCE (GVarCE op) opr1) opr2) =
  case lookup op prim2 of
    Just inst -> compileWHNF f opr2 ++ compileWHNF (pushStack f 1) opr1 ++ [inst]
    Nothing -> compileLazy f e ++ [Eval]
compileWHNF f e = compileLazy f e ++ [Eval]

-- TODO: lazy case: generate a function (lambda lifting)
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

compiledPrimFn :: [CompiledCoreFn]
compiledPrimFn = compiledPrimFns ++
  [ ("and", 2, [Push 0, Eval, Jump [(0, [Pop 1, Pack 0 0]), (1, [Pop 1, Push 1, Eval])], Update 2, Pop 2]),
    ("or",  2, [Push 0, Eval, Jump [(0, [Pop 1, Push 1, Eval]), (1, [Pop 1, Pack 1 0])], Update 2, Pop 2])
  ]
-- and x y = case x of { False -> False; True -> y };
-- or x y = case x of { True -> True; False -> y };

type CompiledCore = ([CompiledCoreConstr], [CompiledCoreFn])

compile :: CoreProgram -> CompiledCore
compile (cs, fs) = (map compileConstr cs, compiledPrimFn ++ map compileFn fs)
