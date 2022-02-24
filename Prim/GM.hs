module Prim.GM where

import Utils.Map
import Common.Def
import GM.Def
import Prim.Name

primInt2Insts :: [(String, Instruction)]
primInt2Insts =
  [ ("add", Add),
    ("sub", Sub),
    ("mul", Mul),
    ("div", Div),
    ("rem", Rem)
  ]

primBool2Insts :: [(String, Instruction)]
primBool2Insts =
  [ ("eq", IsEq),
    ("lt", IsLt),
    ("gt", IsGt),
    ("ne", IsNe),
    ("le", IsLe),
    ("ge", IsGe)
  ]

primBool1Insts :: [(String, Instruction)]
primBool1Insts =
  [ ("not", Not)
  ]

prim2Insts :: [(String, Instruction)]
prim2Insts = primInt2Insts ++ primBool2Insts

prim1Insts :: [(String, Instruction)]
prim1Insts = primBool1Insts

compilePrimFn :: Int -> (String, Instruction) -> (Name, Int, Code)
compilePrimFn arity (name, inst) =
  (primNameMap ! name, arity,
    concat (replicate arity [Push (arity - 1), Eval]) ++ [inst, Update arity, Pop arity])

compiledPrimFns :: [(Name, Int, Code)]
compiledPrimFns =
  -- and x y = case x of { False -> False; True -> y };
  -- or x y = case x of { True -> True; False -> y };
  [ (primNameMap ! "and", 2, [Push 0, Eval, CaseJ [(0, [Pop 1, Pack 0 0]), (1, [Pop 1, Push 1, Eval])], Update 2, Pop 2])
  , (primNameMap ! "or",  2, [Push 0, Eval, CaseJ [(0, [Pop 1, Push 1, Eval]), (1, [Pop 1, Pack 1 0])], Update 2, Pop 2])
  ]
  ++ map (compilePrimFn 2) prim2Insts
  ++ map (compilePrimFn 1) prim1Insts
