module Core.Def where

import qualified Data.Set as S

import Common.Def

-- Core is not strongly typed
data CoreExpr
  = GFnCE Name
  | GConstrCE Name
  | LiftedFn Ident
  | LVarCE Ident
  | IntCE  Int
  | HNFCE CoreConstr [CoreExpr]        -- saturated constructor
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

localVarsExcept :: S.Set Ident -> CoreExpr -> S.Set Ident
localVarsExcept _ (GFnCE _) = S.empty
localVarsExcept _ (GConstrCE _) = S.empty
localVarsExcept _ (LiftedFn _) = S.empty
localVarsExcept _ (IntCE _) = S.empty

localVarsExcept eSet (LVarCE id) =
  if id `S.member` eSet then S.empty else S.singleton id

localVarsExcept eSet (HNFCE c es) =
  foldl S.union S.empty (map (localVarsExcept eSet) es)

localVarsExcept eSet (AppCE e1 e2) =
  locals e1 `S.union` locals e2
  where locals = localVarsExcept eSet

localVarsExcept eSet (CaseCE e brs) =
  foldl S.union (localVarsExcept eSet e) (map localsBr brs)
  where
    localsBr (_, brBinds, body) =
      let newESet = foldl (flip S.insert) eSet brBinds
       in localVarsExcept newESet body

localVarsExcept eSet (LetCE b e) =
  let (bindId, bindExpr) = b
      newESet = S.insert bindId eSet
   in localVarsExcept eSet bindExpr `S.union`
      localVarsExcept newESet e

localVarsExcept eSet (LetRecCE bs e) =
  let locals = localVarsExcept newESet
      (bindIds, bindExprs) = unzip bs
      newESet = foldl (flip S.insert) eSet bindIds
   in foldl S.union (locals e) (map locals bindExprs)

localVarsExcept eSet (LambdaCE ps e) =
  localVarsExcept (foldl (flip S.insert) eSet ps) e

replaceLocal :: Ident -> CoreExpr -> CoreExpr -> CoreExpr
replaceLocal id replE e = case e of
  LVarCE id' | id == id' -> replE
  HNFCE c es ->
    HNFCE c (map replace es)
  AppCE e1 e2 ->
    AppCE (replace e1) (replace e2)
  CaseCE e brs ->
    CaseCE (replace e) (map replaceBr brs)
  LetCE bind e ->
    LetCE (replaceBind bind) (replace e)
  LetRecCE binds e ->
    LetRecCE (map replaceBind binds) (replace e)
  LambdaCE params e ->
    LambdaCE params (replace e)
  _ -> e
  where
    replace = replaceLocal id replE
    replaceBr (tag, binds, e) = (tag, binds, replace e)
    replaceBind (id, e) = (id, replace e)