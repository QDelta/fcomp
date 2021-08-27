module Type.Core where

-- Core is not strongly typed
-- typechecker desugars the source and generate core

data CoreExpr
  = GVarCE String
  | LVarCE Int
  | IntCE  Int
  | AppCE CoreExpr CoreExpr
  | CaseCE CoreExpr [CoreBranch]
  deriving (Show)

type CoreBranch = (Int, Int, CoreExpr)  -- arity, tag, body
type CoreConstr = (String, Int, Int)    -- name, arity, tag
type CoreFn = (String, Int, CoreExpr)   -- name, arity, body

type CoreProgram = ([CoreConstr], [CoreFn])

pprint :: CoreProgram -> String
pprint = show
