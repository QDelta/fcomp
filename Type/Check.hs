module Type.Check where

import Utils
import Parser.AST
import Type.Def
import Type.Core

type FNameMap = Map String Type
type TNameMap = Map String Type
type CNameMap = Map String Type
type TypeEnv = (FNameMap, CNameMap, TNameMap) -- function, constructor, type
type LNameMap = Map String Int

boolT :: Type
boolT = DataT "Bool"

listT :: Type
listT = DataT "List"

primFunc :: FNameMap
primFunc = mFromList 
  [ ("+", FnT IntT (FnT IntT IntT)),
    ("-", FnT IntT (FnT IntT IntT)),
    ("*", FnT IntT (FnT IntT IntT)),
    ("div", FnT IntT (FnT IntT IntT)),
    ("rem", FnT IntT (FnT IntT IntT)),
    ("=?", FnT IntT (FnT IntT boolT)),
    (">?", FnT IntT (FnT IntT boolT)),
    ("<?", FnT IntT (FnT IntT boolT)),
    ("and", FnT boolT (FnT boolT boolT)),
    ("or", FnT boolT (FnT boolT boolT)),
    ("not", FnT boolT boolT)
  ]

-- (data Bool False True)
-- (data List Nil (Cons Int List))

primConstr :: CNameMap
primConstr = mFromList
  [ ("False", boolT),
    ("True", boolT),
    ("Nil", listT),
    ("Cons", FnT IntT (FnT listT listT))
  ]

primType :: TNameMap
primType = mFromList
  [ ("Int", IntT),
    ("List", listT),
    ("Bool", boolT)
  ]

initialTypeEnv :: TypeEnv
initialTypeEnv = (primFunc, primConstr, primType)

initialCore :: CoreProgram 
initialCore = 
  ([("False", 0, 0), ("True", 0, 1), ("Nil", 0, 0), ("Cons", 2, 1)], [])

typeSigToType :: TNameMap -> TypeSig -> Type
typeSigToType tNM (AtomTS n) = mLookup tNM n
typeSigToType tNM (ArrowTS ts1 ts2) = FnT (typeSigToType tNM ts1) (typeSigToType tNM ts2)

-- retType -> [t1,...,tn] -> (t1->...->tn->retType)
multiFnT :: Type -> [Type] -> Type
multiFnT = foldr FnT

addConstr :: TNameMap -> CNameMap -> (Type, Constructor) -> CNameMap
addConstr tNM fNM (dType, (name, tSigs))
  = mInsert fNM (name, multiFnT dType (map (typeSigToType tNM) tSigs))

addCoreConstr :: [Constructor] -> Int -> [CoreConstr] -> [CoreConstr]
addCoreConstr [] _ x = x
addCoreConstr ((name, tSigs) : rest) tag cCons = 
  addCoreConstr rest (tag + 1) newCCons
  where newCCons = (name, length tSigs, tag) : cCons

typeCheckStmt :: (TypeEnv, CoreProgram) -> Statement -> (TypeEnv, CoreProgram)
typeCheckStmt ((fNM, cNM, tNM), (cCons, cFn)) (DataDSTMT (name, constrs)) =
  ((fNM, newCNM, newTNM), (newCCons, cFn))
  where
    dType = DataT name
    newTNM = mInsert tNM (name, dType)
    newCNM = foldl (addConstr newTNM) cNM (zip (repeat dType) constrs)
    newCCons = addCoreConstr constrs 0 cCons

typeCheckStmt ((fNM, cNM, tNM), (cCons, cFn)) (DeclSTMT (name, typeSig)) =
  ((mInsert fNM (name, fType), cNM, tNM), (cCons, cFn))
  where fType = typeSigToType tNM typeSig

typeCheckStmt ((fNM, cNM, tNM), (cCons, cFn)) (FnDSTMT (name, params, body)) =
  ((fNM', cNM, tNM), (cCons, newCFn))
  where
    fType = mLookup fNM name
    (fNMTemp, expectBType) = paramBind (fNM, fType) params
    actualBType = typeCheckExpr cNM fNMTemp body
    fNM' = if actualBType == expectBType then fNM else error "Type Error: Fn"
    lNM = mFromList (zip params [0..])
    cBody = genCoreExpr cCons lNM body
    newCFn = (name, length params, cBody) : cFn

paramBind :: (FNameMap, Type) -> [String] -> (FNameMap, Type)
paramBind = foldl paramBind1

paramBind1 :: (FNameMap, Type) -> String -> (FNameMap, Type)
paramBind1 (fNM, fType) name = case fType of
  FnT t1 t2 -> (mInsert fNM (name, t1), t2)
  _ -> error "Type Error: Fn"

genCoreExpr :: [CoreConstr] -> LNameMap -> Expr -> CoreExpr
genCoreExpr _ _ (IntLitE n) = IntCE n
genCoreExpr _ lNM (VarE name) = 
  case mLookupMaybe lNM name of
    Just i -> LVarCE i
    Nothing -> GVarCE name
genCoreExpr cCons lNM (ApE e1 e2) = AppCE (gen e1) (gen e2)
  where gen = genCoreExpr cCons lNM
genCoreExpr cCons lNM (CaseE e brs) = CaseCE (gen e) coreBrs
  where
    gen = genCoreExpr cCons lNM
    coreBrs = map (genCoreBranch cCons lNM) brs

genCoreBranch :: [CoreConstr] -> LNameMap -> Branch -> (Int, Int, CoreExpr)
genCoreBranch cCons lNM (name : binds, e) = (arity, tag, ce)
  where
    atMap = mFromList (map (\(n, a, t) -> (n, (a, t))) cCons)
    (arity, tag) = mLookup atMap name
    lNMTemp = foldl mInsert lNM (zip binds [(mCount lNM)..])
    ce = genCoreExpr cCons lNMTemp e

-- TODO: exhaustive pattern

typeCheckExpr :: CNameMap -> FNameMap -> Expr -> Type
typeCheckExpr cNM fNM expr = case expr of
  IntLitE _ -> IntT
  VarE name -> case mLookupMaybe fNM name of
    Just t -> t
    Nothing -> mLookup cNM name
  ApE e1 e2 -> case typeCheckExpr cNM fNM e1 of
    FnT t11 t12 -> case typeCheckExpr cNM fNM e2 of
      t2 | t2 == t11 -> t12
         | otherwise -> error errMsg
    _ -> error errMsg
    where errMsg = "Type Error: expr applicaton"
  CaseE e bs -> finalType
    where 
      eType = typeCheckExpr cNM fNM e
      brTPairs = map (typeCheckBranch cNM fNM) bs
      finalType = typeCheckAllBr eType brTPairs

-- pattern type, body expr type
typeCheckBranch :: CNameMap -> FNameMap -> Branch -> (Type, Type)
typeCheckBranch cNM fNM (pat, body) = (pType, bType)
  where
    (fNMTemp, pType) = patBind cNM fNM pat
    bType = typeCheckExpr cNM fNMTemp body

patBind :: CNameMap -> FNameMap -> Pattern -> (FNameMap, Type)
patBind cNM fNM (name : binds) = paramBind (fNM, mLookup cNM name) binds

typeCheckAllBr :: Type -> [(Type, Type)] -> Type
typeCheckAllBr eType brTPairs =
  if properCase then allEqType bTypes else error "Type Error: constructor type"
  where
    (pTypes, bTypes) = unzip brTPairs
    properCase = all (== eType) pTypes
    allEqType (h : t) = if all (== h) t then h else error "Type Error: expression in case"
    allEqType [] = error "Type Error: No branches"

typeCheckProgram :: Program -> CoreProgram
typeCheckProgram prog = cp'
  where 
    (te, cp) = foldl typeCheckStmt (initialTypeEnv, initialCore) prog
    cp' = if length (show te) > 1 then cp else error "" -- just to force typecheck
