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

type GlobalInfo = (Map Ident Int, Set Ident) -- constructor tag, global name

genCore :: Program Name -> CoreProgram
genCore (dataDefs, fnDefs) =
  (allConstrs, coreFns)
  where
    allConstrs = primConstrs ++ defConstrs
    defConstrs = concatMap genConstrs dataDefs
    cMap = genConstrInfo allConstrs
    gSet = genGIdents fnDefs defConstrs `sUnion` primIdents
    gInfo = (cMap, gSet)
    coreFns = map (genFn gInfo) fnDefs

genConstrInfo :: [CoreConstr] -> Map Ident Int
genConstrInfo =
  foldl (\m (n, a, t) -> mInsert (getIdent n, t) m) emptyMap

genGIdents :: [FnDef Name] -> [CoreConstr] -> Set Ident
genGIdents fnDefs constrs =
           foldl (\s (FnDef n _ _) -> sInsert s (getIdent n)) emptySet fnDefs
  `sUnion` foldl (\s (n, _, _) -> sInsert s (getIdent n)) emptySet constrs

genConstrs :: DataDef Name -> [CoreConstr]
genConstrs (DataDef name _ constrs) =
  zipWith genC constrs [0..]
  where
    genC (name, tSigs) tag = (name, length tSigs, tag)

genFn :: GlobalInfo -> FnDef Name -> CoreFn
genFn gInfo (FnDef name params body) =
  (name, map getIdent params, genExpr gInfo body)

genExpr :: GlobalInfo -> Expr Name -> CoreExpr
genExpr _ (IntE n) = IntCE n
genExpr (_, gSet) (VarE name) =
  let id = getIdent name in
  if id `sElem` gSet then
    GVarCE name
  else
    LVarCE id
genExpr gInfo (AppE e1 e2) =
  AppCE (gen e1) (gen e2)
  where gen = genExpr gInfo
genExpr gInfo (CaseE e brs) =
  CaseCE ce coreBrs
  where
    ce = genExpr gInfo e
    coreBrs = map (genBranch gInfo) brs
genExpr gInfo (LetE binds e) =
  foldr
    (\grp ce ->
      case grp of
        AcyclicSCC b -> LetCE b ce
        CyclicSCC bs -> LetRecCE bs ce)
    (genExpr gInfo e)
    bindCEGrps
  where
    identSet = sFromList (map (getIdent . fst) binds)
    depGraph =
      map
      (\(n, e) ->
        ((n, e), getIdent n, sToList $ depsOfExprIn identSet e))
      binds
    bindExprGrps = strongCCs depGraph
    bindCEGrps =
      map (fmap (bimap getIdent (genExpr gInfo))) bindExprGrps
genExpr gInfo (LambdaE params e) =
  LambdaCE (map getIdent params) (genExpr gInfo e)

genBranch :: GlobalInfo -> Branch Name -> CoreBranch
genBranch gInfo@(cMap, _) (name, binds, e) =
  (tag, map getIdent binds, genExpr gInfo e)
  where
    tag = cMap ! getIdent name

primConstrs :: [CoreConstr]
primConstrs =
  map
    (\(s, arity, tag) ->
      (primNameMap ! s, arity, tag))
    primCoreConstrs

primIdents :: Set Ident
primIdents = sFromList (enumFromTo minPrimIdent maxPrimIdent)