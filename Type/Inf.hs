module Type.Inf where

import Utils.Function
import Utils.Map
import Parser.AST
import Type.Def

type FTMap = Map String MType
type DTMap = Map String MType
type CTMap = Map String MType
type VTMap = Map Int MType

data NameEnv = NameEnv
  { fm :: FTMap,  -- functions
    dm :: DTMap,  -- data types
    cm :: CTMap   -- constructors
  }

data InferEnv = InferEnv
  {  vm :: VTMap, -- type variables
    cnt :: Int    -- unique id generator
  }

type TypeEnv = (NameEnv, InferEnv)

-- TODO: state monad

newType :: InferEnv -> (MType, InferEnv)
newType env = (VarT id, env {cnt = id + 1})
  where id = cnt env

resolve :: InferEnv -> MType -> MType
resolve env (VarT p) = 
  case mLookupMaybe (vm env) p of
    Just (VarT p) -> resolve env (VarT p)
    Just t -> t
    Nothing -> VarT p
resolve _ t = t

unify :: InferEnv -> (MType, MType) -> InferEnv
unify env (l, r) = 
  case (resolve env l, resolve env r) of
    (VarT lp,    rt        ) -> bindVar env (lp, rt)
    (lt,         VarT rp   ) -> bindVar env (rp, lt)
    (ArrT ll lr, ArrT rl rr) ->
      unify (unify env (ll, rl)) (lr, rr)
    (lt,         rt        ) -> 
      if lt == rt then env else error $ "can not unify " ++ show lt ++ " with " ++ show rt

bindVar :: InferEnv -> (Int, MType) -> InferEnv
bindVar env (p1, VarT p2) | p1 == p2 = env
bindVar env (p, t) = env {vm = mInsert (vm env) (p, t)}

bindF :: NameEnv -> (String, MType) -> NameEnv
bindF env (name, t) = env {fm = mInsert (fm env) (name, t)}

bindC :: NameEnv -> (String, MType) -> NameEnv
bindC env (name, t) = env {cm = mInsert (cm env) (name, t)}

bindD :: NameEnv -> (String, MType) -> NameEnv
bindD env (name, t) = env {dm = mInsert (dm env) (name, t)}

inferExpr :: NameEnv -> InferEnv -> Expr -> (MType, InferEnv)
inferExpr ne ie expr = case expr of
  IntE _ -> (IntT, ie)
  VarE name -> (t, ie)
    where 
      t = mLookup (fm ne) name $
        mLookup (cm ne) name (error $ "undefined " ++ name)
  AppE l r -> (ret, newIE)
    where
      (lt, ie1) = inferE ie l
      (rt, ie2) = inferE ie1 r
      (ret, ie3) = newType ie2
      newIE = unify ie3 (ArrT rt ret, lt)
  CaseE expr brs -> (brType, newIE)
    where
      (eType, ie1) = inferE ie expr
      (brType, ie2) = newType ie1
      (tPairs, ie3) = mapAccumL (inferBr ne) ie2 brs
      (patTypes, actualBrTs) = unzip tPairs
      ie4 = foldl unify ie3 (zip (repeat eType) patTypes)
      newIE = foldl unify ie4 (zip (repeat brType) actualBrTs)
  where inferE = inferExpr ne

-- (type of pattern, type of body)
inferBr :: NameEnv -> InferEnv -> ([String], Expr) -> ((MType, MType), InferEnv)
inferBr ne ie (constr : params, body) = ((assertDataT pType, bType), newIE)
  where
    consType = mLookup (cm ne) constr (error $ "undefined constructor " ++ constr)
    (binds, pType) = paramBind params consType
    tmpNE = foldl bindF ne binds
    (bType, newIE) = inferExpr tmpNE ie body
    assertDataT t@(DataT _) = t
    assertDataT _ = error "invalid pattern"

-- params -> function type -> (binds, return type)
paramBind :: [String] -> MType -> ([(String, MType)], MType)
paramBind [] t = ([], t)
paramBind (n : ns) (ArrT t1 t2) = ((n, t1) : restBind, ret)
  where (restBind, ret) = paramBind ns t2

multiArrT :: MType -> [MType] -> MType
multiArrT = foldr ArrT

constructDataDef :: TypeEnv -> DataDef -> TypeEnv
constructDataDef (ne, ie) (DataDef name constrs) = (newNE, ie)
  where
    newNE = ne `bindD` (name, DataT name)

constructFnDef :: TypeEnv -> FnDef -> TypeEnv
constructFnDef (ne, ie) (FnDef name params body) = (newNE, newIE)
  where
    (retType, ie1) = newType ie
    (paramTypes, newIE) = mapAccumL (\ie _ -> newType ie) ie1 params
    fType = multiArrT retType paramTypes
    newNE = ne `bindF` (name, fType)

inferDataDef :: TypeEnv -> DataDef -> TypeEnv
inferDataDef (ne, ie) (DataDef name constrs) = (newNE, ie)
  where
    dType = DataT name
    newNE = foldl inferConstr ne (zip (repeat dType) constrs)
    inferConstr :: NameEnv -> (MType, Constructor) -> NameEnv
    inferConstr env (dType, (name, tss)) = newEnv
      where
        cType = multiArrT dType (map (typeSigToType env) tss)
        newEnv = env `bindC` (name, cType)
    typeSigToType :: NameEnv -> TypeSig -> MType
    typeSigToType env (AtomTS name) = mLookup (dm env) name (error $ "undefined type " ++ name)
    typeSigToType env (ArrTS ts1 ts2) = ArrT (typeSigToType env ts1) (typeSigToType env ts2)

inferFnDef :: TypeEnv -> FnDef -> TypeEnv
inferFnDef (ne, ie) (FnDef name params body) = (ne, newIE)
  where
    fType = mLookup (fm ne) name (error $ "can not find type of " ++ name)
    (binds, retType) = paramBind params fType
    tmpNE = foldl bindF ne binds
    (bType, ie1) = inferExpr tmpNE ie body
    newIE = unify ie1 (retType, bType)
    
infer :: Program -> String
infer (dataDefs, fnDefs) = envStr
  where 
    e1 = foldl constructDataDef initialTypeEnv dataDefs
    e2 = foldl constructFnDef e1 fnDefs
    e3 = foldl inferDataDef e2 dataDefs
    e4 = foldl inferFnDef e3 fnDefs
    envStr = envShow e4

recResolve :: InferEnv -> MType -> MType
recResolve ie (VarT p) =
  case mLookupMaybe (vm ie) p of
    Just t -> recResolve ie t
    Nothing -> VarT p
recResolve ie (ArrT t1 t2) = ArrT (recResolve ie t1) (recResolve ie t2) 
recResolve _ t = t

resolveMap :: InferEnv -> Map String MType -> Map String MType
resolveMap ie = fmap $ recResolve ie

envShow :: TypeEnv -> String
envShow (ne, ie) =
  "Functions: \n" ++ show (resolveMap ie (fm ne)) ++ "\n\n" ++
  "Data types: \n" ++ show (resolveMap ie (dm ne)) ++ "\n\n" ++
  "Constructors: \n" ++ show (resolveMap ie (cm ne)) ++ "\n"

initialNameEnv :: NameEnv
initialNameEnv = NameEnv
  { fm = primFn,
    dm = primData,
    cm = primConstr
  }

initialInferEnv :: InferEnv
initialInferEnv = InferEnv
  {  vm = emptyMap,
    cnt = 0
  }

initialTypeEnv :: TypeEnv
initialTypeEnv = (initialNameEnv, initialInferEnv)

boolT, listT :: MType
boolT = DataT "Bool"
listT = DataT "List"

primFn :: FTMap
primFn = mFromList
  [ ("add", ArrT IntT  (ArrT IntT IntT)),
    ("sub", ArrT IntT  (ArrT IntT IntT)),
    ("mul", ArrT IntT  (ArrT IntT IntT)),
    ("div", ArrT IntT  (ArrT IntT IntT)),
    ("rem", ArrT IntT  (ArrT IntT IntT)),
    ("eq",  ArrT IntT  (ArrT IntT boolT)),
    ("gt",  ArrT IntT  (ArrT IntT boolT)),
    ("lt",  ArrT IntT  (ArrT IntT boolT)),
    ("ne",  ArrT IntT  (ArrT IntT boolT)),
    ("ge",  ArrT IntT  (ArrT IntT boolT)),
    ("le",  ArrT IntT  (ArrT IntT boolT)),
    ("and", ArrT boolT (ArrT boolT boolT)),
    ("or",  ArrT boolT (ArrT boolT boolT)),
    ("not", ArrT boolT boolT)
  ]

primData :: DTMap
primData = mFromList
  [ ("Int",  IntT),
    ("List", listT),
    ("Bool", boolT)
  ]

primConstr :: CTMap
primConstr = mFromList
  [ ("False", boolT),
    ("True",  boolT),
    ("Nil",   listT),
    ("Cons",  ArrT IntT (ArrT listT listT))
  ]
