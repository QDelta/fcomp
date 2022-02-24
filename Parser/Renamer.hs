module Parser.Renamer (rename) where

import Utils.Function
import Utils.Env
import Utils.State
import Common.Def
import Common.AST
import Prim.Name

type RenameEnv = (Env RdrName Name, Ident) -- env, next ident

type RState a = State RenameEnv a

primEnv :: RenameEnv
primEnv = (eFromList primNames, succ maxPrimIdent)

findName :: RdrName -> RState Name
findName s = State $
  \(e, id) -> (e ! s, (e, id))

findNameMaybe :: RdrName -> RState (Maybe Name)
findNameMaybe s = State $
  \(e, id) -> (eLookup s e, (e, id))

bindName :: RdrName -> RState Name
bindName s = State $
  \(e, id) -> let name = Name (s, id) in
    (name, (eBind (s, name) e, succ id))

rmBind :: RdrName -> RState ()
rmBind n = State $
  \(e, id) -> ((), (eRemove n e, id))

rename :: Program RdrName -> Program Name
rename prog = evalState (renameProg prog) primEnv

renameProg :: Program RdrName -> RState (Program Name)
renameProg (dataDefs, fnDefs) =
  case redefName of
    Just s -> error $ "redefinition of " ++ s
    Nothing -> do
      traverse_ bindName (dataConstrs ++ fnNames)
      rDatas <- traverse renameData dataDefs
      rFns <- traverse renameFn fnDefs
      return (rDatas, rFns)
  where
    dataNames = map (\(DataDef n _ _) -> n) dataDefs
    dataConstrs = map fst $ concatMap (\(DataDef _ _ cs) -> cs) dataDefs
    fnNames = map (\(FnDef n _ _) -> n) fnDefs
    redefName =
      checkUnique dataNames <|> checkUnique dataConstrs <|> checkUnique fnNames
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Just a  <|> _ = Just a
    Nothing <|> b = b

renameData :: DataDef RdrName -> RState (DataDef Name)
renameData (DataDef name tps constrs) = do
  rConstrs <- traverse renameConstr constrs
  return $ DataDef name tps rConstrs
  where
    renameConstr (name, tSigs) = do
      rName <- findName name
      return (rName, tSigs)

renameFn :: FnDef RdrName -> RState (FnDef Name)
renameFn (FnDef fName params body) =
  case checkUnique params of
    Just s -> error $ "duplicate parameter" ++ s
    Nothing -> do
      rFName <- findName fName
      rParams <- traverse bindName params
      rBody <- renameExpr body
      traverse_ rmBind params
      return $ FnDef rFName rParams rBody

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
renameExpr (CaseE e brs) = do
  re <- renameExpr e
  rBrs <- traverse renameBr brs
  return $ CaseE re rBrs
  where
    renameBr (cons, bindNames, body) =
      case checkUnique bindNames of
        Just s -> error $ "duplicate bind" ++ s
        Nothing -> do
          rCons <- findName cons
          rBindNames <- traverse bindName bindNames
          rBody <- renameExpr body
          traverse_ rmBind bindNames
          return (rCons, rBindNames, rBody)
renameExpr (LetE binds e) =
  case checkUnique bindNames of
    Just s -> error $ "duplicate bind" ++ s
    Nothing -> do
      rBindNames <- traverse bindName bindNames
      rBindEs <- traverse renameExpr bindEs
      re <- renameExpr e
      traverse_ rmBind bindNames
      return $ LetE (zip rBindNames rBindEs) re
  where
    (bindNames, bindEs) = unzip binds
renameExpr (LambdaE params e) =
  case checkUnique params of
    Just s -> error $ "duplicate parameter" ++ s
    Nothing -> do
      rParams <- traverse bindName params
      re <- renameExpr e
      traverse_ rmBind params
      return $ LambdaE rParams re