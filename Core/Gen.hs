module Core.Gen (genCore) where

import Utils.Function
import Utils.Map
import Utils.Set
import Utils.Graph
import Common.Def
import Common.AST
import Type.Inf
import Core.Def
import Prim.Name
import Prim.Core

genCore :: Program Name -> CoreProgram
genCore (dataDefs, fnDefs) =
  foldl addCoreFn (foldl addCoreData initialCore dataDefs) fnDefs

addCoreData :: CoreProgram -> DataDef Name -> CoreProgram
addCoreData (cCons, cFn) (DataDef name _ constrs) =
  (addCoreConstrs constrs 0 cCons, cFn)

addCoreConstrs :: [Constructor Name] -> Int -> [CoreConstr] -> [CoreConstr]
addCoreConstrs [] _ x = x
addCoreConstrs ((name, tSigs) : rest) tag cCons =
  addCoreConstrs rest (tag + 1) newCCons
  where newCCons = (name, length tSigs, tag) : cCons

addCoreFn :: CoreProgram -> FnDef Name -> CoreProgram
addCoreFn (cCons, cFn) (FnDef name params body) =
  (cCons, (name, length params, cBody) : cFn)
  where
    cBody = genCoreExpr cCons lm body
    lm = mFromList $ zip (map getIdent params) [0..]

type LOffSetMap = Map Ident Int -- (local var, offset)

genCoreExpr :: [CoreConstr] -> LOffSetMap -> Expr Name -> CoreExpr
genCoreExpr _ _ (IntE n) = IntCE n
genCoreExpr _ lm (VarE name) =
  case mLookup (getIdent name) lm of
    Just i -> LVarCE i
    Nothing -> GVarCE name
genCoreExpr cCons lm (AppE e1 e2) =
  AppCE (gen e1) (gen e2)
  where gen = genCoreExpr cCons lm
genCoreExpr cCons lm (CaseE e brs) =
  CaseCE ce coreBrs
  where
    ce = genCoreExpr cCons lm e
    coreBrs = map (genCoreBranch cCons lm) brs
genCoreExpr cCons lm (LetE binds e) =
  undefined
  where
    (names, bindExprs) = unzip binds
    nameSet = sFromList (map getIdent names)
    test = map (\(n, e) -> (e, n, depOf e)) binds
    depOf :: Expr Name -> Set Ident
    depOf (IntE _) = emptySet
    depOf (VarE name) =
      let id = getIdent name in
      if id `sElem` nameSet then sSingleton id else emptySet
    depOf (AppE e1 e2) = depOf e1 `sUnion` depOf e2
    depOf (CaseE e brs) =
      depOf e `sUnion` undefined

genCoreBranch :: [CoreConstr] -> LOffSetMap -> Branch Name -> CoreBranch
genCoreBranch cCons lm (name, binds, e) = (arity, tag, ce)
  where
    atMap = mFromList $ map (\(n, a, t) -> (n, (a, t))) cCons
    (arity, tag) = assertJust $ mLookup name atMap
    tmpLM = foldr mInsert lm (zip (map getIdent binds) [mSize lm..])
    ce = genCoreExpr cCons tmpLM e

initialCore :: CoreProgram
initialCore = (initialCoreConstrs, [])
  where
    initialCoreConstrs =
      map
        (\(s, arity, tag) ->
          (primNames ! s, arity, tag))
        primCoreConstrs
