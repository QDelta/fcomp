module Core.Gen (genCore) where

import Utils.Function
import Utils.Map
import Parser.AST
import Type.Inf
import Core.Def

initialCore :: CoreProgram
initialCore =
  ([("False", 0, 0), ("True", 0, 1), ("Nil", 0, 0), ("Cons", 2, 1)], [])

genCore :: Program -> CoreProgram
genCore (dataDefs, fnDefs) =
  foldl addCoreFn (foldl addCoreData initialCore dataDefs) fnDefs

addCoreData :: CoreProgram -> DataDef -> CoreProgram
addCoreData (cCons, cFn) (DataDef name _ constrs) =
  (addCoreConstrs constrs 0 cCons, cFn)

addCoreConstrs :: [Constructor] -> Int -> [CoreConstr] -> [CoreConstr]
addCoreConstrs [] _ x = x
addCoreConstrs ((name, tSigs) : rest) tag cCons =
  addCoreConstrs rest (tag + 1) newCCons
  where newCCons = (name, length tSigs, tag) : cCons

addCoreFn :: CoreProgram -> FnDef -> CoreProgram
addCoreFn (cCons, cFn) (FnDef name params body) =
  (cCons, (name, length params, cBody) : cFn)
  where
    cBody = genCoreExpr cCons lm body
    lm = mFromList $ zip params [0..]

type LOffSetMap = Map String Int -- (local var, offset)

genCoreExpr :: [CoreConstr] -> LOffSetMap -> Expr -> CoreExpr
genCoreExpr _ _ (IntE n) = IntCE n
genCoreExpr _ lm (VarE name) =
  case mLookup name lm of
    Just i -> LVarCE i
    Nothing -> GVarCE name
genCoreExpr cCons lm (AppE e1 e2) = AppCE (gen e1) (gen e2)
  where gen = genCoreExpr cCons lm
genCoreExpr cCons lm (CaseE e brs) =
  CaseCE ce coreBrs
  where
    ce = genCoreExpr cCons lm e
    coreBrs = map (genCoreBranch cCons lm) brs

genCoreBranch :: [CoreConstr] -> LOffSetMap -> Branch -> CoreBranch
genCoreBranch cCons lm (name, binds, e) = (arity, tag, ce)
  where
    atMap = mFromList $ map (\(n, a, t) -> (n, (a, t))) cCons
    (arity, tag) = assertJust $ mLookup name atMap
    tmpLM = foldr mInsert lm (zip binds [length lm..])
    ce = genCoreExpr cCons tmpLM e
