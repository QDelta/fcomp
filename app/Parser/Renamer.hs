module Parser.Renamer (rename) where

import Utils.Function
import Utils.Env
import Utils.Map
import Utils.State
import Common.Def
import Common.AST
import Prim.Name

data RenameEnv = RenameEnv
  { renv :: Env RdrName Name -- env
  , next :: Ident            -- ident generator
  , cmap :: Map Ident Int    -- constr to number of constrs in the data
  }

type RState a = State RenameEnv a

primEnv :: RenameEnv
primEnv = RenameEnv
  { renv = eFromList primNames
  , next = succ maxPrimIdent
  , cmap = primBranchNum
  }

findName :: RdrName -> RState Name
findName s = State $
  \e -> (renv e Utils.Env.! s, e)

findNameMaybe :: RdrName -> RState (Maybe Name)
findNameMaybe s = State $
  \e -> (eLookup s (renv e), e)

findBranchNum :: Ident -> RState Int
findBranchNum c = State $
  \e -> (cmap e Utils.Map.! c, e)

bindName :: RdrName -> RState Name
bindName s = State $ \e ->
  let id = next e in
  let name = Name (s, id) in
    ( name
    , RenameEnv
        { renv = eBind (s, name) (renv e)
        , next = succ id
        , cmap = cmap e
        }
    )

rmBind :: RdrName -> RState ()
rmBind n = State $ \e ->
  ( ()
  , RenameEnv
      { renv = eRemove n (renv e)
      , next = next e
      , cmap = cmap e
      }
  )

bindBranchNum :: (Ident, Int) -> RState ()
bindBranchNum (constr, n) = State $ \e ->
  ( ()
  , RenameEnv
      { renv = renv e
      , next = next e
      , cmap = mInsert (constr, n) (cmap e)
      }
  )

rename :: Program RdrName -> Program Name
rename prog = evalState (renameProg prog) primEnv

renameProg :: Program RdrName -> RState (Program Name)
renameProg (dataGrps, valGroups) =
  case redefName of
    Just s -> error $ "redefinition of " ++ s
    Nothing -> do
      rDataGrps <- traverse renameDataGroup dataGrps
      rGroups <- traverse renameGroup valGroups
      return (rDataGrps, rGroups)
  where
    dataNames = concatMap getDataNames dataGrps
    dataConstrs = concatMap getConstrNames dataGrps
    redefName = checkUnique dataNames <|> checkUnique dataConstrs
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Just a  <|> _ = Just a
    Nothing <|> b = b

renameDataGroup :: DataGroup RdrName -> RState (DataGroup Name)
renameDataGroup (RecData dataBinds) = do
  rDataBinds <- traverse bindData dataBinds
  return $ RecData rDataBinds
  where
    bindData (name, tps, constrs) = do
      case checkUnique tps of
        Just tp -> error $ "duplicate type parameter " ++ tp
        Nothing -> do
          rConstrs <- traverse bindConstr constrs
          return (name, tps, rConstrs)
      where
        nConstrs = length constrs
        bindConstr (name, tsigs) = do
          rName <- bindName name
          bindBranchNum (getIdent rName, nConstrs)
          return (rName, tsigs)

renameGroup :: ValGroup RdrName -> RState (ValGroup Name)
renameGroup (ValDef (name, expr)) = do
  rExpr <- renameExpr expr
  rName <- bindName name
  return $ ValDef (rName, rExpr)
renameGroup (RecVal binds) = do
  rNames <- traverse bindName names
  rExprs <- traverse renameExpr exprs
  return $ RecVal (zip rNames rExprs)
  where
    (names, exprs) = unzip binds

renameExpr :: Expr RdrName -> RState (Expr Name)
renameExpr (IntE n) =
  return $ IntE n
renameExpr (VarE s) = do
  name <- findNameMaybe s
  case name of
    Just name -> return $ VarE name
    Nothing -> error $ "undefined name " ++ s
renameExpr (AppE e1 e2) = do
  re1 <- renameExpr e1
  re2 <- renameExpr e2
  return $ AppE re1 re2
renameExpr (CaseE e brs) =
  case checkUnique brConstrs of
    Just cons -> error $ "duplicate branches of " ++ cons
    Nothing -> do
      re <- renameExpr e
      rBrs <- traverse renameBr brs
      expectBranchNum <- findBranchNum ((getIdent . fst3 . head) rBrs)
      if length brs == expectBranchNum
      then return $ CaseE re rBrs
      else error "incorrect branch number"
  where
    brConstrs = map fst3 brs
    renameBr (cons, bindNames, body) =
      case checkUnique bindNames of
        Just s -> error $ "duplicate case bind " ++ s
        Nothing -> do
          rCons <- findName cons
          rBindNames <- traverse bindName bindNames
          rBody <- renameExpr body
          traverse_ rmBind bindNames
          return (rCons, rBindNames, rBody)
renameExpr (LetE (name, bindE) e) = do
  rBindE <- renameExpr bindE
  rBindName <- bindName name
  re <- renameExpr e
  rmBind name
  return $ LetE (rBindName, rBindE) re
renameExpr (LetRecE binds e) =
  case checkUnique bindNames of
    Just s -> error $ "duplicate rec bind " ++ s
    Nothing -> do
      rBindNames <- traverse bindName bindNames
      rBindEs <- traverse renameExpr bindEs
      re <- renameExpr e
      traverse_ rmBind bindNames
      return $ LetRecE (zip rBindNames rBindEs) re
  where
    (bindNames, bindEs) = unzip binds
renameExpr (LambdaE params e) =
  case checkUnique params of
    Just s -> error $ "duplicate lambda parameter " ++ s
    Nothing -> do
      rParams <- traverse bindName params
      re <- renameExpr e
      traverse_ rmBind params
      return $ LambdaE rParams re