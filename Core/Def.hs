module Core.Def where

-- Core is not strongly typed

data CoreExpr
  = GVarCE String
  | LVarCE Int
  | IntCE  Int
  | AppCE CoreExpr CoreExpr
  | CaseDCE CoreExpr [CoreBranch]
  | CaseICE CoreExpr [CoreBranch]
  deriving (Show)

type CoreBranch = (Int, Int, CoreExpr)  -- arity, tag, body
type CoreConstr = (String, Int, Int)    -- name, arity, tag
type CoreFn = (String, Int, CoreExpr)   -- name, arity, body

type CoreProgram = ([CoreConstr], [CoreFn])
