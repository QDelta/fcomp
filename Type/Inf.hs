module Type.Inf (infer) where

import Data.Graph (SCC, stronglyConnComp, flattenSCC)
import Data.Foldable (traverse_)

import Utils.Function
import Utils.Map
import Utils.Set
import Utils.State
import Parser.AST
import Type.Def

-- data type with only constant constructors can be translate to integers
type DTMap = Map String (Int, Bool) -- number of type parameters, is number type
type FTMap = Map String PType
type CTMap = Map String PType
type LTMap = Map String MType
type VTMap = Map Int MType

data TypeEnv = TypeEnv
  { dtmap :: DTMap -- global data types
  , ftmap :: FTMap -- global functions
  , ctmap :: CTMap -- global constructors
  , ltmap :: LTMap -- local variables
  , vtmap :: VTMap -- equations for type variables
  , index :: Int   -- unique id generator
  }

instance Show TypeEnv where
  show env = 
    "Data types:\n" ++ show (dtmap env) ++ "\n\n" ++
    "Functions:\n" ++ show (ftmap env) ++ "\n\n" ++
    "Constructors:\n" ++ show (ctmap env) ++ "\n"

initialTypeEnv :: TypeEnv
initialTypeEnv = TypeEnv
  { dtmap = primData
  , ftmap = primFn
  , ctmap = primConstr
  , ltmap = emptyMap
  , vtmap = emptyMap
  , index = 0
  }

type TState a = State TypeEnv a

typeError :: String -> TState a
typeError = error

clearInfer :: TState ()
clearInfer = State $
  \env -> ((), env { ltmap = emptyMap, vtmap = emptyMap, index = 0 })

getEnv :: (TypeEnv -> a) -> TState a
getEnv f = State $
  \env -> (f env, env)

-- get : (Map -> a) -> TState a
getD f = getEnv (f . dtmap)
getF f = getEnv (f . ftmap)
getC f = getEnv (f . ctmap)
getL f = getEnv (f . ltmap)
getV f = getEnv (f . vtmap)

-- set : (Map -> Map) -> TState ()
setD f = State $ 
  \env -> ((), env { dtmap = f (dtmap env) })
setF f = State $ 
  \env -> ((), env { ftmap = f (ftmap env) })
setC f = State $ 
  \env -> ((), env { ctmap = f (ctmap env) })
setL f = State $ 
  \env -> ((), env { ltmap = f (ltmap env) })
setV f = State $ 
  \env -> ((), env { vtmap = f (vtmap env) })

bindD p = setD (mInsert p)
bindF p = setF (mInsert p)
bindC p = setC (mInsert p)
bindL p = setL (mInsert p)
bindV p = setV (mInsert p)

newTypeVar :: TState MType
newTypeVar = State $
  \env -> let id = index env
  in (VarT id, env { index = id + 1 })

newTypeVars :: Int -> TState [MType]
newTypeVars n = traverse (const newTypeVar) (replicate n ())

depGroupSort :: Set String -> [FnDef] -> [[FnDef]]
depGroupSort constrNames defs = map flattenSCC sccs
  where
    depGraph = genDepGraph constrNames defs
    sccs = stronglyConnComp depGraph

-- (f1, f2) in E : f1 (indirectly) calls f2
genDepGraph :: Set String -> [FnDef] -> [(FnDef, String, [String])]
genDepGraph constrNames = map genDep
  where
    genDep def@(FnDef name params body) =
      (def, name, sToList $ directDepsExpr knowns body)
      where
        knowns = sUnion constrNames (sFromList params)

directDepsExpr :: Set String -> Expr -> Set String
directDepsExpr locals expr = case expr of
  IntE _ -> emptySet
  VarE name -> if name `sElem` locals then emptySet else sSingleton name
  AppE l r -> sUnion (deps l) (deps r)
  CaseE e brs -> sUnion (deps e) (directDepsBranches locals brs)
  where deps = directDepsExpr locals

directDepsBranches :: Set String -> [Branch] -> Set String
directDepsBranches locals brs = 
  foldl sUnion emptySet (map dDepBr brs)
  where
    dDepBr :: Branch -> Set String
    dDepBr (constr, params, expr) = 
      directDepsExpr (sUnion locals (sFromList params)) expr

exConstrs :: [DataDef] -> Set String
exConstrs defs = foldl sUnion emptySet (map ex defs)
  where ex (DataDef _ _ constrs) = sFromList $ map fst constrs

infer :: Program -> String
infer prog = show env
  where env = execState (inferProgram prog) initialTypeEnv

inferProgram :: Program -> TState ()
inferProgram (dataDefs, fnDefs) = do
  traverse_ constructData dataDefs
  traverse_ inferData dataDefs
  let constrNames = exConstrs dataDefs
  let fnGrps = depGroupSort constrNames fnDefs
  traverse_ inferGroup fnGrps

constructData :: DataDef -> TState ()
constructData (DataDef name tparams constrs) =
  bindD (name, (length tparams, all (null . snd) constrs))

inferData :: DataDef -> TState ()
inferData (DataDef name tpNames constrs) = do
  traverse_ inferConstr constrs
  where
    tparams = take (length tpNames) [0..]
    tpMap = mFromList (zip tpNames tparams)
    dType = DataT name (map VarT tparams)

    inferConstr :: Constructor -> TState ()
    inferConstr (name, tss) = do
      types <- traverse tsToType tss
      let cType = Forall (sFromList tparams) (foldr ArrT dType types)
      bindC (name, cType)

    tsToType :: TypeSig -> TState MType
    tsToType (VarTS name) = 
      case mLookup name tpMap of
        Just p -> return $ VarT p
        Nothing -> typeError $ "undefined type parameter " ++ name
    tsToType (ArrTS l r) = do
      lt <- tsToType l
      rt <- tsToType r
      return (ArrT lt rt)
    tsToType (DataTS name tss) = do
      maybeArity <- getD (mLookup name)
      arity <- case maybeArity of
        Just (a, _) -> return a
        Nothing -> typeError $ "undefined data type " ++ name
      name' <- if arity == length tss then return name else typeError "incomplete data type"
      types <- traverse tsToType tss
      return (DataT name' types)

inferGroup :: [FnDef] -> TState ()
inferGroup group = do
  inferGroupM group
  let names = map (\(FnDef name _ _) -> name) group
  mtypes <- traverse (getL . (\n -> assertJust . mLookup n)) names
  ptypes <- traverse generalize mtypes
  traverse_ bindF $ zip names ptypes
  clearInfer

inferGroupM :: [FnDef] -> TState ()
inferGroupM group = do
  traverse_ constrFn group
  traverse_ inferFn group

constrFn :: FnDef -> TState ()
constrFn (FnDef name params _) = do
  ptypes <- newTypeVars (length params)
  retType <- newTypeVar
  let fType = foldr ArrT retType ptypes
  bindL (name, fType)

inferFn :: FnDef -> TState ()
inferFn (FnDef name params body) = do
  fType <- getL (assertJust . mLookup name)
  let (binds, retType) = paramBind params fType
  traverse_ bindL binds
  bType <- inferExpr body
  unify retType bType
  traverse_ (setL . mRemove) params

inferExpr :: Expr -> TState MType
inferExpr (IntE _) = return mIntT
inferExpr (VarE name) = do
  maybel <- getL (mLookup name)
  case maybel of
    Just t -> return t
    Nothing -> do
      maybef <- getF (mLookup name)
      case maybef of
        Just t -> instantiate t
        Nothing -> do
          maybec <- getC (mLookup name)
          case maybec of
            Just t -> instantiate t
            Nothing -> typeError $ "undefined name " ++ name
inferExpr (AppE l r) = do
  lt <- inferExpr l
  rt <- inferExpr r
  appT <- newTypeVar
  unify lt (ArrT rt appT)
  return appT
inferExpr (CaseE expr brs) = do
  eType <- inferExpr expr
  brType <- newTypeVar
  tPairs <- traverse inferBr brs
  let (patTypes, actualBrTs) = unzip tPairs
  traverse_ (unify eType) patTypes
  traverse_ (unify brType) actualBrTs
  return brType

inferBr :: Branch -> TState (MType, MType)
inferBr (constr, params, body) = do
  maybep <- getC (mLookup constr)
  cTypeP <- case maybep of
    Just t -> return t
    Nothing -> typeError $ "undefined constructor " ++ constr
  cType <- instantiate cTypeP
  let (binds, patType) = paramBind params cType
  traverse_ bindL binds
  bType <- inferExpr body
  traverse_ (setL . mRemove) params
  return (assertDataT patType, bType)
  where
    assertDataT t@(DataT _ _) = t
    assertDataT _ = error "pattern with non-data type"

-- params -> function type -> (binds, return type)
paramBind :: [String] -> MType -> ([(String, MType)], MType)
paramBind [] t = ([], t)
paramBind (n : ns) (ArrT t1 t2) = ((n, t1) : restBind, ret)
  where (restBind, ret) = paramBind ns t2
paramBind _ _ = error ""

deepResolve :: MType -> TState MType
deepResolve (VarT p) = do
  maybet <- getV (mLookup p)
  case maybet of
    Just t -> deepResolve t
    Nothing -> return (VarT p)
deepResolve (ArrT l r) = do
  lt <- deepResolve l
  rt <- deepResolve r
  return (ArrT lt rt)
deepResolve (DataT n ts) = do
  rts <- traverse deepResolve ts
  return (DataT n rts)

resolve :: MType -> TState MType
resolve (VarT p) = do
  maybet <- getV (mLookup p)
  case maybet of
    Just (VarT p') -> resolve (VarT p')
    Just t -> return t
    Nothing -> return $ VarT p
resolve t = return t

instantiate :: PType -> TState MType
instantiate (Forall pset t) = do
  let plist = sToList pset
  tparams <- newTypeVars (length plist)
  return $ substitute (mFromList $ zip plist tparams) t

substitute :: Map Int MType -> MType -> MType
substitute smap t = case t of
  VarT p -> case mLookup p smap of
    Just t -> t
    Nothing -> VarT p
  ArrT l r -> ArrT (subst l) (subst r)
  DataT n ts -> DataT n (map subst ts)
  where subst = substitute smap

unify :: MType -> MType -> TState ()
unify l r = do
  lt <- resolve l
  rt <- resolve r
  case (lt, rt) of
    (VarT p, t) -> unifyP p t
    (t, VarT p) -> unifyP p t
    (ArrT ll lr, ArrT rl rr) -> do
      unify ll rl
      unify lr rr
    (DataT n1 ts1, DataT n2 ts2) ->
      if n1 == n2 then
        traverse_ (uncurry unify) (zip ts1 ts2)
      else
        unifyError n1 n2
    (lt, rt) -> unifyError (show lt) (show rt)

unifyP :: Int -> MType -> TState ()
unifyP p t = case t of
  VarT p1 | p == p1 -> return ()
  t | recCheck p t -> unifyError (show p) (show t)
    | otherwise -> bindV (p, t)
  where
    recCheck p (VarT p1) = p == p1
    recCheck p (ArrT l r) = recCheck p l || recCheck p r
    recCheck p (DataT _ ts) = foldl (\b t -> b || recCheck p t) False ts

unifyError s1 s2 = typeError $ "can not unify " ++ s1 ++ " with " ++ s2

generalize :: MType -> TState PType
generalize t = do
  rt <- deepResolve t
  return (Forall (freeVarsR rt) rt)
  where
    freeVarsR :: MType -> Set Int
    freeVarsR (VarT p) = sSingleton p
    freeVarsR (ArrT l r) = freeVarsR l `sUnion` freeVarsR r
    freeVarsR (DataT n ts) = foldl sUnion emptySet (map freeVarsR ts)

mIntT, mBoolT :: MType
mIntT = DataT "Int" []
mBoolT = DataT "Bool" []

primFn :: FTMap
primFn = mFromList
  [ ("add", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("sub", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("mul", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("div", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("rem", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("eq",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("gt",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("lt",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("ne",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("ge",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("le",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("or",  Forall emptySet $ ArrT mBoolT (ArrT mBoolT mBoolT)),
    ("and", Forall emptySet $ ArrT mBoolT (ArrT mBoolT mBoolT)),
    ("not", Forall emptySet $ ArrT mBoolT mBoolT)
  ]

primData :: DTMap
primData = mFromList
  [ ("Int",  (0, True)),
    ("Bool", (0, True)),
    ("List", (1, False))
  ]

primConstr :: CTMap
primConstr = mFromList
  [ ("False", Forall emptySet mBoolT),
    ("True",  Forall emptySet mBoolT),
    ("Nil",   Forall (sSingleton 0) $ DataT "List" [VarT 0]),
    ("Cons",  Forall (sSingleton 0) $ ArrT (VarT 0) $ ArrT (DataT "List" [VarT 0]) (DataT "List" [VarT 0]))
  ]
