module Type.Inf (infer) where

import Utils.Function
import Utils.Map
import Utils.Set
import Utils.State
import Common.Def
import Common.AST
import Type.Def
import Prim.Name
import Prim.Type

type DataInfo = (String, DataAttr)
type ValInfo = (Name, PType)

data TypeEnv = TypeEnv
  { dtmap :: Map String DataAttr  -- global data types
  , ftmap :: Map Ident PType      -- global functions
  , ctmap :: Map Ident PType      -- global constructors
  , ltmap :: Map Ident MType      -- local variables
  , vtmap :: Map Int MType        -- equations for type variables
  , index :: Int                  -- typevar generator
  }

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
setC f = State $
  \env -> ((), env { ctmap = f (ctmap env) })
setF f = State $
  \env -> ((), env { ftmap = f (ftmap env) })
setL f = State $
  \env -> ((), env { ltmap = f (ltmap env) })
setV f = State $
  \env -> ((), env { vtmap = f (vtmap env) })

bindD p = setD (mInsert p)
bindC p = setC (mInsert p)
bindF p = setF (mInsert p)
bindL p = setL (mInsert p)
bindV p = setV (mInsert p)

removeL k = setL (mRemove k)

newTypeVar :: TState MType
newTypeVar = State $
  \env -> let id = index env
  in (VarT id, env { index = id + 1 })

newTypeVars :: Int -> TState [MType]
newTypeVars n = traverse (const newTypeVar) (replicate n ())

entryName :: RdrName
entryName = "start"

entryType :: MType
entryType = ArrT mIntT (DataT "List" [mIntT])

checkEntry :: (Name, MType) -> TState ()
checkEntry (name, mtype) =
  if getRdrName name == entryName
  then unify entryType mtype
  else return ()

infer :: Program Name -> String
infer prog =
  (concat . interleave "\n")
    (map showData dataInfos ++ map showVal valInfos)
  where
    (dataInfos, valInfos) = evalState (inferProgram prog) initialTypeEnv
    showData (name, (arity, _)) =
      name ++ " :: " ++ concat (replicate arity "* -> ") ++ "*"
    showVal (name, t) =
      getRdrName name ++ " : " ++ show t

inferProgram :: Program Name -> TState ([DataInfo], [ValInfo])
inferProgram (dataGrps, groups) = do
  dataInfoLs <- traverse inferDataGroup dataGrps
  valInfoLs <- traverse inferGroup groups
  return (concat dataInfoLs, concat valInfoLs)

inferDataGroup :: DataGroup Name -> TState [DataInfo]
inferDataGroup (RecData binds) = do
  dataInfos <- traverse addData binds
  traverse_ inferData binds
  return dataInfos
  where
    addData :: DataBind Name -> TState DataInfo
    addData (name, tpNames, constrs) = do
      bindD (name, attr)
      return (name, attr)
      where
        attr = (length tpNames, all (null . snd) constrs)

    inferData :: DataBind Name -> TState ()
    inferData (name, tpNames, constrs) =
      traverse_ inferConstr constrs
      where
        tparams = take (length tpNames) [0..]
        tpMap = mFromList (zip tpNames tparams)
        dType = DataT name (map VarT tparams)

        inferConstr :: Constructor Name -> TState ()
        inferConstr (name, tss) = do
          types <- traverse tsToType tss
          let cType = Forall (sFromList tparams) (foldr ArrT dType types)
          bindC (getIdent name, cType)

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
          name' <-
            if arity == length tss
            then return name
            else typeError "incomplete data type"
          types <- traverse tsToType tss
          return (DataT name' types)

inferGroup :: ValGroup Name -> TState [ValInfo]
inferGroup (ValDef bind) = do
  info <- inferValGroup bind
  return [info]
inferGroup (RecVal binds) =
  inferRecGroup binds

inferValGroup :: Bind Name -> TState ValInfo
inferValGroup (name, expr) = do
  (ident, mtype) <- inferValGroupM (name, expr)
  checkEntry (name, mtype)
  ptype <- generalize mtype
  removeL ident
  bindF (ident, ptype)
  return (name, ptype)

inferValGroupM :: Bind Name -> TState (Ident, MType)
inferValGroupM (name, expr) = do
  let ident = getIdent name
  eType <- inferExpr expr
  bindL (ident, eType)
  return (ident, eType)

inferRecGroup :: [Bind Name] -> TState [ValInfo]
inferRecGroup group = do
  (idents, mtypes) <- unzip <$> inferRecGroupM group
  let names = map fst group
  traverse_ checkEntry (zip names mtypes)
  ptypes <- traverse generalize mtypes
  traverse_ removeL idents
  traverse_ bindF (zip idents ptypes)
  return $ zip names ptypes

inferRecGroupM :: [Bind Name] -> TState [(Ident, MType)]
inferRecGroupM group = do
  traverse_ constrBind group
  traverse inferBind group
  where
    constrBind (name, _) = do
      bType <- newTypeVar
      bindL (getIdent name, bType)
    inferBind (name, expr) = do
      let ident = getIdent name
      bType <- getL (! ident)
      eType <- inferExpr expr
      unify bType eType
      return (ident, bType)

inferExpr :: Expr Name -> TState MType
inferExpr (IntE _) = return mIntT
inferExpr (VarE name) = do
  let id = getIdent name
  let lookup = mLookup id
  maybel <- getL lookup
  case maybel of
    Just t -> return t
    Nothing -> do
      maybef <- getF lookup
      case maybef of
        Just t -> instantiate t
        Nothing -> do
          maybec <- getC lookup
          case maybec of
            Just t -> instantiate t
            Nothing -> typeError $ "undefined name " ++ getRdrName name
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
inferExpr (LetE bind e) = do
  bIdent <- fst <$> inferValGroupM bind
  eType <- inferExpr e
  removeL bIdent
  return eType
inferExpr (LetRecE binds e) = do
  bIdents <- map fst <$> inferRecGroupM binds
  eType <- inferExpr e
  traverse_ removeL bIdents
  return eType
inferExpr (LambdaE params e) = do
  ptypes <- newTypeVars (length params)
  traverse_ bindL (zip (map getIdent params) ptypes)
  eType <- inferExpr e
  return $ foldr ArrT eType ptypes

inferBr :: Branch Name -> TState (MType, MType)
inferBr (constr, params, body) = do
  maybep <- getC (mLookup (getIdent constr))
  cTypeP <- case maybep of
    Just t -> return t
    Nothing -> typeError $ "undefined constructor " ++ getRdrName constr
  cType <- instantiate cTypeP
  let paramIdents = map getIdent params
  let (paramTypes, patType) = paramBind paramIdents cType
  traverse_ bindL (zip paramIdents paramTypes)
  bType <- inferExpr body
  traverse_ removeL paramIdents
  return (assertDataT patType, bType)
  where
    assertDataT t@(DataT _ _) = t
    assertDataT _ = error "pattern with non-data type"

-- params -> function type -> (param types, return type)
paramBind :: [Ident] -> MType -> ([MType], MType)
paramBind [] t = ([], t)
paramBind (_ : ns) (ArrT t1 t2) = (t1 : rest, ret)
  where (rest, ret) = paramBind ns t2
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
    Just t@(VarT _) -> resolve t
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
    (lt, rt) -> unifyError lt rt

unifyP :: Int -> MType -> TState ()
unifyP p t = case t of
  VarT p1 | p == p1 -> return ()
  t | recCheck p t -> unifyError p t
    | otherwise -> bindV (p, t)
  where
    recCheck p (VarT p1) = p == p1
    recCheck p (ArrT l r) = recCheck p l || recCheck p r
    recCheck p (DataT _ ts) = foldl (\b t -> b || recCheck p t) False ts

unifyError :: (Show a, Show b) => a -> b -> TState x
unifyError t1 t2 =
  typeError $ "can not unify " ++ show t1 ++ " with " ++ show t2

generalize :: MType -> TState PType
generalize t = do
  rt <- deepResolve t
  return (Forall (freeVarsR rt) rt)
  where
    freeVarsR :: MType -> Set Int
    freeVarsR (VarT p) = sSingleton p
    freeVarsR (ArrT l r) = freeVarsR l `sUnion` freeVarsR r
    freeVarsR (DataT n ts) = foldl sUnion emptySet (map freeVarsR ts)

primData :: Map String DataAttr
primData = primDataAttrs

primFn :: Map Ident PType
primFn =
  foldl
    (\m (s, typ) ->
      mInsert (getIdent (primNameMap ! s), typ) m)
    emptyMap
    (mToList primFnTypes)

primConstr :: Map Ident PType
primConstr =
  foldl
    (\m (s, typ) ->
      mInsert (getIdent (primNameMap ! s), typ) m)
    emptyMap
    (mToList primConstrTypes)