module Type where

import Utils
import AST

data Type
  = IntT
  | DataT String
  | FnT Type Type
  deriving (Show, Eq)

type FNameMap = Map String Type
type TNameMap = Map String Type
type CNameMap = Map String Type
type Env = (FNameMap, CNameMap, TNameMap) -- function, constructor, type

primFunc :: FNameMap
primFunc = mFromList
  [ ("+", FnT IntT (FnT IntT IntT)),
    ("-", FnT IntT (FnT IntT IntT)),
    ("*", FnT IntT (FnT IntT IntT)),
    ("/", FnT IntT (FnT IntT IntT))
  ]

primConstr :: CNameMap
primConstr = emptyMap 

primType :: TNameMap
primType = mFromList [("Int", IntT)]

initialEnv :: Env
initialEnv = (primFunc, primConstr, primType)

typeSigToType :: TNameMap -> TypeSig -> Type
typeSigToType tNM (AtomTS n) = mLookUp tNM n
typeSigToType tNM (ArrowTS ts1 ts2) = FnT (typeSigToType tNM ts1) (typeSigToType tNM ts2)

-- retType -> [t1,...,tn] -> (t1->...->tn->retType)
multiFnT :: Type -> [Type] -> Type
multiFnT = foldr FnT

addConstr :: TNameMap -> FNameMap -> (Type, Constructor) -> FNameMap
addConstr tNM fNM (dType, (name, tSigs))
  = mInsert fNM (name, multiFnT dType (map (typeSigToType tNM) tSigs))

typeCheckStmt :: Env -> Statement -> (Env, Type)
typeCheckStmt (fNM, cNM, tNM) (DataDSTMT (name, constrs))
  = ((fNM, newCNM, newTNM), dType)
  where
    dType = DataT name
    newTNM = mInsert tNM (name, dType)
    newCNM = foldl (addConstr newTNM) cNM (zip (repeat dType) constrs)

typeCheckStmt (fNM, cNM, tNM) (DeclSTMT (name, typeSig))
  = ((mInsert fNM (name, fType), cNM, tNM), fType)
  where fType = typeSigToType tNM typeSig

typeCheckStmt (fNM, cNM, tNM) (FnDSTMT (name, params, body))
  = ((fNM', cNM, tNM), fType)
  where
    fType = mLookUp fNM name
    (fNMTemp, expectBType) = paramBind (fNM, fType) params
    actualBType = typeCheckExpr cNM fNMTemp body
    fNM' = if actualBType == expectBType then fNM else error "Type Error: Fn"

paramBind :: (FNameMap, Type) -> [String] -> (FNameMap, Type)
paramBind = foldl paramBind1

paramBind1 :: (FNameMap, Type) -> String -> (FNameMap, Type)
paramBind1 (fNM, fType) name = case fType of
  FnT t1 t2 -> (mInsert fNM (name, t1), t2)
  _ -> error "Type Error: Fn"

typeCheckExpr :: CNameMap -> FNameMap -> Expr -> Type
typeCheckExpr cNM fNM expr = case expr of
  ILitE _ -> IntT
  VarE name -> case mLookUpMaybe fNM name of
    Just t -> t
    Nothing -> mLookUp cNM name
  ApE e1 e2 -> case typeCheckExpr cNM fNM e1 of
    FnT t11 t12 -> case typeCheckExpr cNM fNM e2 of
      t2 | t2 == t11 -> t12
         | otherwise -> error errMsg
    _ -> error errMsg
    where errMsg = "Type Error: expr applicaton"
  CaseE e bs -> finalType
    where 
      eType = typeCheckExpr cNM fNM e
      brTPairs = map (typeCheckBranch cNM fNM eType) bs
      finalType = typeCheckAllBr eType brTPairs

-- pattern type, body expr type
typeCheckBranch :: CNameMap -> FNameMap -> Type -> Branch -> (Type, Type)
typeCheckBranch cNM fNM eType (pat, body) = (pType, bType)
  where
    (fNMTemp, pType) = patBind cNM fNM eType pat
    bType = typeCheckExpr cNM fNMTemp body

patBind :: CNameMap -> FNameMap -> Type -> Pattern -> (FNameMap, Type)
patBind _ fNM eType [name] = (mInsert fNM (name, eType), eType)
patBind cNM fNM _ (name : binds) = paramBind (fNM, mLookUp cNM name) binds

typeCheckAllBr :: Type -> [(Type, Type)] -> Type
typeCheckAllBr eType brTPairs
  = if properCase then allEqType bTypes else error "Type Error: constructor type"
  where
    (pTypes, bTypes) = unzip brTPairs
    properCase = all (== eType) pTypes
    allEqType (h : t) = if all (== h) t then h else error "Type Error: expression in case"
    allEqType [] = error "Type Error: No branches"

typeCheckProgram :: Program -> (Env, [Type])
typeCheckProgram = mapAccumL typeCheckStmt initialEnv

