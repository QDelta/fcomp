module Parser.Renamer (rename) where

import Utils.Function
import Utils.Map
import Utils.Set
import Utils.State
import Common.Def
import Common.AST
import Prim.Name

type RenameEnv = ([(RdrName, Name)], Ident) -- map, next ident

type RState a = State RenameEnv a

primEnv :: RenameEnv
primEnv = (mToList primNames, mSize primNames)

findName :: RdrName -> RState (Maybe Name)
findName s = State $
  \(m, id) -> (lookup s m, (m, id))

bindName :: RdrName -> RState Name
bindName s = State $
  \(m, id) -> let name = Name (s, id) in
    (name, ((s, name) : m, id + 1))

rmRecent :: Int -> RState ()
rmRecent n = State $
  \(m, id) -> ((), (drop n m, id))

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
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Just a  <|> _ = Just a
    Nothing <|> b = b
    redefName =
      checkUnique dataNames <|> checkUnique dataConstrs <|> checkUnique fnNames

renameData :: DataDef RdrName -> RState (DataDef Name)
renameData (DataDef name tps constrs) = do
  rConstrs <- traverse renameConstr constrs
  return $ DataDef name tps rConstrs
  where
    renameConstr (name, tSigs) = do
      rName <- assertJust <$> findName name
      return (rName, tSigs)

renameFn :: FnDef RdrName -> RState (FnDef Name)
renameFn (FnDef fName params body) =
  case checkUnique params of
    Just s -> error $ "duplicate parameter" ++ s
    Nothing -> do
      rFName <- assertJust <$> findName fName
      rParams <- traverse bindName params
      rBody <- renameExpr body
      rmRecent (length params)
      return $ FnDef rFName rParams rBody

renameExpr :: Expr RdrName -> RState (Expr Name)
renameExpr (IntE n) =
  return $ IntE n
renameExpr (VarE s) = do
  name <- findName s
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
          rmRecent (length bindNames)
          return (assertJust rCons, rBindNames, rBody)
renameExpr (LetE binds e) =
  case checkUnique bindNames of
    Just s -> error $ "duplicate bind" ++ s
    Nothing -> do
      rBindNames <- traverse bindName bindNames
      rBindEs <- traverse renameExpr bindEs
      re <- renameExpr e
      rmRecent (length bindNames)
      return $ LetE (zip rBindNames rBindEs) re
  where
    (bindNames, bindEs) = unzip binds
renameExpr (LambdaE params e) =
  case checkUnique params of
    Just s -> error $ "duplicate parameter" ++ s
    Nothing -> do
      rParams <- traverse bindName params
      re <- renameExpr e
      rmRecent (length params)
      return $ LambdaE rParams re
