module Core.Def where

import Common.Def

-- Core is not strongly typed
data CoreExpr
  = GVarCE Name
  | LVarCE Ident
  | IntCE  Int
  | AppCE CoreExpr CoreExpr
  | CaseCE CoreExpr [CoreBranch]
  | LetCE CoreBind CoreExpr            -- bind, expression
  | LetRecCE [CoreBind] CoreExpr       -- rec binds, expresstion
  deriving (Show)

type CoreBind = (Ident, CoreExpr)
type CoreBranch = (Int, [Ident], CoreExpr) -- tag, binds, body
type CoreConstr = (Name, Int, Int)         -- name, arity, tag
type CoreFn = (Name, [Ident], CoreExpr)    -- name, params, body

type CoreProgram = ([CoreConstr], [CoreFn])
