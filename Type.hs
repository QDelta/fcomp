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
typeSigToType _ [] = error "Empty type signature"
typeSigToType tNM [n1] = mLookUp tNM n1
typeSigToType tNM (n1 : rn) = FnT (mLookUp tNM n1) (typeSigToType tNM rn)

-- retT -> [t1,...,tn] -> (t1->...->tn->retT)
multiFnT :: Type -> [Type] -> Type
multiFnT = foldr FnT

addConstr :: TNameMap -> FNameMap -> (Type, Constructor) -> FNameMap
addConstr tNM fNM (dType, (name, tSigs))
  = mInsert fNM (name, multiFnT dType (map (typeSigToType tNM) tSigs))

typeCheckDef :: Env -> Definition -> (Env, Type)
typeCheckDef (fNM, cNM, tNM) (DataDef name constrs)
  = ((fNM, newCNM, newTNM), dType)
  where
    dType = DataT name
    newTNM = mInsert tNM (name, dType)
    newCNM = foldl (addConstr newTNM) cNM (zip (repeat dType) constrs)

typeCheckDef (fNM, cNM, tNM) (FnDef name params body)
  = ((newFNM, cNM, tNM), fType)
  where
    paramCount = length params
    paramsWithT = map (second (typeSigToType tNM)) params
    fNMTemp = foldl mInsert fNM paramsWithT
    eType = typeCheckExpr cNM fNMTemp body
    fType = multiFnT eType (map snd paramsWithT)
    newFNM = mInsert (mDrop paramCount fNMTemp) (name, fType)

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
    where errMsg = "Invalid application: " ++ show e1 ++ " " ++ show e2
  CaseE e bs -> error ""
    where eType = typeCheckExpr cNM fNM expr

patBind :: FNameMap -> String -> [String] -> (Type, FNameMap)
patBind fNM constrName
  = multiFnBind (mLookUp fNM constrName)

multiFnBind :: Type -> [String] -> (Type, FNameMap)
multiFnBind t (n1 : ns) = case t of
  FnT t1 t2 -> second (`mInsert` (n1, t1)) (multiFnBind t2 ns) 
  _ -> error "Invalid constructor"
multiFnBind t [] = case t of
  DataT _ -> (t, emptyMap)
  _ -> error "Invalid constructor"

