module Core.Def where

import Common.Def

-- Core is not strongly typed
data CoreExpr
  = GVarCE Name
  | LVarCE Int
  | IntCE  Int
  | AppCE CoreExpr CoreExpr
  | CaseCE CoreExpr [CoreBranch]
  | LetCE CoreExpr CoreExpr            -- bind, expression
  | LetRecCE [CoreExpr] CoreExpr       -- rec binds, expresstion
  deriving (Show)

type CoreBranch = (Int, Int, CoreExpr) -- arity, tag, body
type CoreConstr = (Name, Int, Int)     -- name, arity, tag
type CoreFn = (Name, Int, CoreExpr)    -- name, arity, body

type CoreProgram = ([CoreConstr], [CoreFn])
