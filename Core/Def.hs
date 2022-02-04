module Core.Def where

import Utils.Set
import Common.Def

-- Core is not strongly typed
data CoreExpr
  = GVarCE Name
  | LiftedFn Ident
  | LVarCE Ident
  | IntCE  Int
  | AppCE CoreExpr CoreExpr
  | CaseCE CoreExpr [CoreBranch]
  | LetCE CoreBind CoreExpr            -- bind, expression
  | LetRecCE [CoreBind] CoreExpr       -- rec binds, expresstion
  | LambdaCE [Ident] CoreExpr
  deriving (Show)

type CoreBind = (Ident, CoreExpr)
type CoreBranch = (Int, [Ident], CoreExpr) -- tag, binds, body
type CoreConstr = (Name, Int, Int)         -- name, arity, tag
type CoreFn = (Name, [Ident], CoreExpr)    -- name, params, body

type CoreProgram = ([CoreConstr], [CoreFn])

localVarsExcept :: Set Ident -> CoreExpr -> Set Ident
localVarsExcept _ (GVarCE _) = emptySet
localVarsExcept _ (IntCE _) = emptySet
localVarsExcept eSet (LVarCE id) =
  if id `sElem` eSet then emptySet else sSingleton id
localVarsExcept eSet (AppCE e1 e2) =
  locals e1 `sUnion` locals e2
  where locals = localVarsExcept eSet
localVarsExcept eSet (CaseCE e brs) =
  foldl sUnion (localVarsExcept eSet e) (map localsBr brs)
  where
    localsBr (_, brBinds, body) =
      localVarsExcept newESet body
      where
        newESet = foldl sInsert eSet brBinds
localVarsExcept eSet (LetCE b e) =
           localVarsExcept eSet bindExpr
  `sUnion` localVarsExcept newESet e
  where
    (bindId, bindExpr) = b
    newESet = sInsert eSet bindId
localVarsExcept eSet (LetRecCE bs e) =
  foldl sUnion (locals e) (map locals bindExprs)
  where
    locals = localVarsExcept newESet
    (bindIds, bindExprs) = unzip bs
    newESet = foldl sInsert eSet bindIds
localVarsExcept eSet (LambdaCE ps e) =
  localVarsExcept (foldl sInsert eSet ps) e
