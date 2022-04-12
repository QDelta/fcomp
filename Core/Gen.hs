module Core.Gen (genCore) where

import Utils.Function
import Utils.Map
import Utils.Set
import Common.Def
import Common.AST
import Type.Inf
import Core.Def
import Prim.Name
import Prim.Core

type GlobalInfo = (Map Ident Int, Set Ident) -- constructor tag, global name

genCore :: Program Name -> CoreProgram
genCore (dataGroups, valGroups) =
  (coreConstrs, coreFns)
  where
    coreConstrs = primConstrs ++ defConstrs
    defConstrs = concatMap genConstrs defDataBinds
    defDataBinds = concatMap (\(RecData l) -> l) dataGroups
    cMap = genConstrInfo coreConstrs
    defValBinds = concatMap getValBinds valGroups
    gSet = genGIdents defValBinds defConstrs `sUnion` primIdents
    gInfo = (cMap, gSet)
    coreFns = map (genFn gInfo) defValBinds

genConstrInfo :: [CoreConstr] -> Map Ident Int
genConstrInfo =
  foldl (\m (n, a, t) -> mInsert (getIdent n, t) m) emptyMap

genGIdents :: [Bind Name] -> [CoreConstr] -> Set Ident
genGIdents fnDefs constrs =
           foldl (\s (n, _) -> sInsert s (getIdent n)) emptySet fnDefs
  `sUnion` foldl (\s (n, _, _) -> sInsert s (getIdent n)) emptySet constrs

genConstrs :: DataBind Name -> [CoreConstr]
genConstrs (name, _, constrs) =
  zipWith genC constrs [0..]
  where
    genC (name, tSigs) tag = (name, length tSigs, tag)

genFn :: GlobalInfo -> Bind Name -> CoreFn
genFn gInfo (name, expr) =
  (name, params, genExpr gInfo body)
  where
    (params, body) = deLam expr
    deLam :: Expr Name -> ([Ident], Expr Name)
    deLam (LambdaE ps expr) = (map getIdent ps, expr)
    deLam expr = ([], expr)

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
genExpr gInfo (LetE bind e) =
  LetCE (genBind gInfo bind) (genExpr gInfo e)
genExpr gInfo (LetRecE binds e) =
  LetRecCE (map (genBind gInfo) binds) (genExpr gInfo e)
genExpr gInfo (LambdaE params e) =
  LambdaCE (map getIdent params) (genExpr gInfo e)

genBind :: GlobalInfo -> Bind Name -> CoreBind
genBind gInfo (n, e) = (getIdent n, genExpr gInfo e)

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