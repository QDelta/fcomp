module Type.Inf (infer) where

import Utils.Function
import Utils.Map
import Utils.Set
import Utils.Graph
import Utils.State
import Common.Def
import Common.AST
import Type.Def
import Prim.Name
import Prim.Type

-- data type with only constant constructors can be translate to integers
type DTMap = Map String DataAttr
type FTMap = Map Ident PType
type CTMap = Map Ident PType
type LTMap = Map Ident MType
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

depGroupSort :: [FnDef Name] -> [[FnDef Name]]
depGroupSort defs = strongCCs (genDepGraph defs)

-- (f1, f2) in E : f1 (indirectly) calls f2
genDepGraph :: [FnDef Name] -> Graph Ident (FnDef Name)
genDepGraph defs = map genDep defs
  where
    fnIds = sFromList $ map (\(FnDef n _ _) -> getIdent n) defs
    genDep def@(FnDef name params body) =
      (def, getIdent name, sToList $ depsOfExprIn fnIds body)

infer :: Program Name -> String
infer prog = show env
  where env = execState (inferProgram prog) initialTypeEnv

inferProgram :: Program Name -> TState ()
inferProgram (dataDefs, fnDefs) = do
  traverse_ constructData dataDefs
  traverse_ inferData dataDefs
  let fnGrps = depGroupSort fnDefs
  traverse_ inferGroup fnGrps

constructData :: DataDef Name -> TState ()
constructData (DataDef name tparams constrs) =
  bindD (name, (length tparams, all (null . snd) constrs))

inferData :: DataDef Name -> TState ()
inferData (DataDef name tpNames constrs) = do
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
      name' <- if arity == length tss then return name else typeError "incomplete data type"
      types <- traverse tsToType tss
      return (DataT name' types)

inferGroup :: [FnDef Name] -> TState ()
inferGroup group = do
  inferGroupM group
  let names = map (\(FnDef name _ _) -> name) group
  mtypes <- traverse (getL . (\n -> (! n)). getIdent) names
  ptypes <- traverse generalize mtypes
  traverse_ bindF $ zip (map getIdent names) ptypes
  clearInfer

inferGroupM :: [FnDef Name] -> TState ()
inferGroupM group = do
  traverse_ constrFn group
  traverse_ inferFn group

constrFn :: FnDef Name -> TState ()
constrFn (FnDef name params _) = do
  ptypes <- newTypeVars (length params)
  retType <- newTypeVar
  let fType = foldr ArrT retType ptypes
  bindL (getIdent name, fType)

inferFn :: FnDef Name -> TState ()
inferFn (FnDef name params body) = do
  fType <- getL (! getIdent name)
  let (binds, retType) = paramBind params fType
  traverse_ bindL binds
  bType <- inferExpr body
  unify retType bType
  traverse_ (setL . mRemove . getIdent) params

inferExpr :: Expr Name -> TState MType
inferExpr (IntE _) = return mIntT
inferExpr (VarE name) = do
  let id = getIdent name
  maybel <- getL (mLookup id)
  case maybel of
    Just t -> return t
    Nothing -> do
      maybef <- getF (mLookup id)
      case maybef of
        Just t -> instantiate t
        Nothing -> do
          maybec <- getC (mLookup id)
          case maybec of
            Just t -> instantiate t
            Nothing -> typeError $ "undefined name " ++ getName name
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
inferExpr (LetE binds e) = do
  bindTypes <- newTypeVars (length binds)
  traverse_ bindL (zip bindIdents bindTypes)
  grpTypes <- traverse (traverse inferExpr) bindExprGrps
  let bindETypes = concat grpTypes
  traverse_ (uncurry unify) (zip bindTypes bindETypes)
  inferExpr e
  where
    (names, bindExprs) = unzip binds
    bindIdents = map getIdent names
    identSet = sFromList bindIdents
    depGraph =
      map
      (\(n, e) ->
        (e, getIdent n, sToList $ depsOfExprIn identSet e))
      binds
    bindExprGrps = strongCCs depGraph

inferBr :: Branch Name -> TState (MType, MType)
inferBr (constr, params, body) = do
  maybep <- getC (mLookup (getIdent constr))
  cTypeP <- case maybep of
    Just t -> return t
    Nothing -> typeError $ "undefined constructor " ++ getName constr
  cType <- instantiate cTypeP
  let (binds, patType) = paramBind params cType
  traverse_ bindL binds
  bType <- inferExpr body
  traverse_ (setL . mRemove . getIdent) params
  return (assertDataT patType, bType)
  where
    assertDataT t@(DataT _ _) = t
    assertDataT _ = error "pattern with non-data type"

-- params -> function type -> (binds, return type)
paramBind :: [Name] -> MType -> ([(Ident, MType)], MType)
paramBind [] t = ([], t)
paramBind (n : ns) (ArrT t1 t2) = ((getIdent n, t1) : restBind, ret)
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

primData :: DTMap
primData = primDataAttrs

primFn :: FTMap
primFn =
  foldl
    (\m (s, typ) ->
      mInsert (getIdent (primNames ! s), typ) m)
    emptyMap
    (mToList primFnTypes)

primConstr :: CTMap
primConstr =
  foldl
    (\m (s, typ) ->
      mInsert (getIdent (primNames ! s), typ) m)
    emptyMap
    (mToList primConstrTypes)
