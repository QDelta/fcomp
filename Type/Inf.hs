module Type.Inf where

import Utils
import Parser.AST
import Type.Def

type FTMap = [(String, Type)]
type DTMap = [(String, Type)]
type CTMap = [(String, Type)]
type VTMap = [(Int, Type)]

data TypeEnv = TypeEnv
  { fm :: FTMap,  -- functions
    dm :: DTMap,  -- data types
    cm :: CTMap,  -- constructors
    vm :: VTMap,  -- type variables
    cnt :: Int    -- unique id
  }

-- TODO: state monad

newType :: TypeEnv -> (Type, TypeEnv)
newType env = (VarT id, env {cnt = id + 1})
  where id = cnt env

newArrType :: TypeEnv -> (Type, TypeEnv)
newArrType env = 
  (ArrT (VarT id) (VarT $ id + 1), env {cnt = id + 2})
  where id = cnt env

resolve :: TypeEnv -> Type -> Type
resolve env (VarT p) = 
  case mLookupMaybe (vm env) p of
    Just (VarT p) -> resolve env (VarT p)
    Just t -> t
    Nothing -> VarT p
resolve _ t = t

unify :: TypeEnv -> (Type, Type) -> TypeEnv
unify env (l, r) = 
  case (resolve env l, resolve env r) of
    (VarT lp,    rt        ) -> bindVar env (lp, rt)
    (lt,         VarT rp   ) -> bindVar env (rp, lt)
    (ArrT ll lr, ArrT rl rr) ->
      unify (unify env (ll, rl)) (lr, rr)
    (lt,         rt        ) -> 
      if lt == rt then env else error $ "can not unify " ++ show lt ++ " " ++ show rt

bindVar :: TypeEnv -> (Int, Type) -> TypeEnv
bindVar env (p1, VarT p2) | p1 == p2 = env
bindVar env (p, t) = env {vm = mInsert (vm env) (p, t)}

bindF :: TypeEnv -> (String, Type) -> TypeEnv
bindF env (name, t) = env {fm = mInsert (fm env) (name, t)}

bindC :: TypeEnv -> (String, Type) -> TypeEnv
bindC env (name, t) = env {cm = mInsert (cm env) (name, t)}

bindD :: TypeEnv -> (String, Type) -> TypeEnv
bindD env (name, t) = env {dm = mInsert (dm env) (name, t)}

inferExpr :: TypeEnv -> Expr -> (Type, TypeEnv)
inferExpr env expr = case expr of
  IntE _ -> (IntT, env)
  VarE name -> (t, env)
    where 
      t = mLookup (fm env) name $
        mLookup (cm env) name (error $ "undefined " ++ name)
  AppE l r -> (ret, newEnv)
    where
      (lt, e1) = inferExpr env l
      (rt, e2) = inferExpr e1 r
      (ret, e3) = newType e2
      newEnv = unify e3 (ArrT rt ret, lt)
  CaseE expr brs -> (brType, newEnv)
    where
      (eType, e1) = inferExpr env expr
      (brType, e2) = newType e1
      (tPairs, e3) = mapAccumL inferBr e2 brs
      (patTypes, actualBrTs) = unzip tPairs
      e4 = foldl unify e3 (zip (repeat eType) patTypes)
      newEnv = foldl unify e4 (zip (repeat brType) actualBrTs)

-- (type of pattern, type of body)
inferBr :: TypeEnv -> ([String], Expr) -> ((Type, Type), TypeEnv)
inferBr env (constr : params, body) = ((assertDataT pType, bType), newEnv)
  where
    consType = mLookup (cm env) constr (error $ "undefined constructor " ++ constr)
    (binds, pType) = paramBind params consType
    tmpE1 = foldl bindF env binds
    (bType, tmpE2) = inferExpr tmpE1 body
    newEnv = env {vm = vm tmpE2, cnt = cnt tmpE2}
    assertDataT t@(DataT _) = t
    assertDataT _ = error "invalid pattern"

-- params -> function type -> (binds, return type)
paramBind :: [String] -> Type -> ([(String, Type)], Type)
paramBind [] t = ([], t)
paramBind (n : ns) (ArrT t1 t2) = ((n, t1) : restBind, ret)
  where (restBind, ret) = paramBind ns t2

multiArrT :: Type -> [Type] -> Type
multiArrT = foldr ArrT

constructDef :: TypeEnv -> Definition -> TypeEnv
constructDef env (FnDef name params body) = newEnv
  where
    (retType, e1) = newType env
    (paramTypes, e2) = mapAccumL (\env _ -> newType env) e1 params
    fType = multiArrT retType paramTypes
    newEnv = e2 `bindF` (name, fType)
constructDef env (DataDef name constrs) = newEnv
  where
    dataType = DataT name
    e1 = foldl constructConstr env (zip (repeat dataType) constrs)
    newEnv = e1 `bindD` (name, dataType)

constructConstr :: TypeEnv -> (Type, Constructor) -> TypeEnv
constructConstr env (dType, (name, tss)) = newEnv
  where
    cType = multiArrT dType (map (typeSigToType env) tss)
    newEnv = env `bindC` (name, cType)

typeSigToType :: TypeEnv -> TypeSig -> Type
typeSigToType env (AtomTS name) = mLookup (dm env) name (error $ "undefined type " ++ name)
typeSigToType env (ArrTS ts1 ts2) = ArrT (typeSigToType env ts1) (typeSigToType env ts2)

inferDef :: TypeEnv -> Definition -> TypeEnv
inferDef env (DataDef _ _) = env
inferDef env (FnDef name params body) = newEnv
  where
    fType = mLookup (fm env) name (error $ "can not find type of " ++ name)
    (binds, retType) = paramBind params fType
    tmpEnv = foldl bindF env binds
    (bType, e1) = inferExpr tmpEnv body
    e2 = unify e1 (retType, bType)
    newEnv = env {vm = vm e2, cnt = cnt e2}

infer :: Program -> String
infer prog = envStr
  where 
    e1 = foldl constructDef initialTypeEnv prog
    e2 = foldl inferDef e1 prog
    envStr = show e2

recResolve :: TypeEnv -> Type -> Type
recResolve env (VarT p) =
  case mLookupMaybe (vm env) p of
    Just (VarT p) -> recResolve env (VarT p)
    Just t -> t
    Nothing -> VarT p
recResolve env (ArrT t1 t2) = ArrT (recResolve env t1) (recResolve env t2) 
recResolve _ t = t

resolveMap :: TypeEnv -> [(String, Type)] -> [(String, Type)]
resolveMap env = map $ second (recResolve env)

instance Show TypeEnv where
  show env =
    "Functions: \n" ++ mShow (resolveMap env (fm env)) ++ "\n\n" ++
    "Data types: \n" ++ mShow (resolveMap env (dm env)) ++ "\n\n" ++
    "Constructors: \n" ++ mShow (resolveMap env (cm env)) ++ "\n"

initialTypeEnv :: TypeEnv
initialTypeEnv = TypeEnv
  { fm = primFn,
    dm = primData,
    cm = primConstr,
    vm = [],
    cnt = 0
  }

boolT, listT :: Type
boolT = DataT "Bool"
listT = DataT "List"

primFn :: FTMap
primFn =
  [ ("+",   ArrT IntT  (ArrT IntT IntT)),
    ("-",   ArrT IntT  (ArrT IntT IntT)),
    ("*",   ArrT IntT  (ArrT IntT IntT)),
    ("div", ArrT IntT  (ArrT IntT IntT)),
    ("rem", ArrT IntT  (ArrT IntT IntT)),
    ("=?",  ArrT IntT  (ArrT IntT boolT)),
    (">?",  ArrT IntT  (ArrT IntT boolT)),
    ("<?",  ArrT IntT  (ArrT IntT boolT)),
    ("and", ArrT boolT (ArrT boolT boolT)),
    ("or",  ArrT boolT (ArrT boolT boolT)),
    ("not", ArrT boolT boolT)
  ]

primData :: DTMap
primData =
  [ ("Int",  IntT),
    ("List", listT),
    ("Bool", boolT)
  ]

primConstr :: CTMap
primConstr =
  [ ("False", boolT),
    ("True",  boolT),
    ("Nil",   listT),
    ("Cons",  ArrT IntT (ArrT listT listT))
  ]
