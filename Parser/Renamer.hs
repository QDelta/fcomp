module Parser.Renamer (rename) where

import Utils.Function
import Utils.Map
import Utils.Set
import Common.Def
import Common.AST
import Prim.Name

type RenameEnv = (Map RdrName Name, Ident) -- map, next ident

primEnv :: RenameEnv
primEnv = (primNames, mSize primNames)

addBind :: RenameEnv -> RdrName -> (Name, RenameEnv)
addBind (nmap, index) s =
  (name, (mInsert (s, name) nmap, index + 1))
  where
    name = Name (s, index)

rename :: Program RdrName -> Program Name
rename (dataDefs, fnDefs) =
  case redefName of
    Just s -> error $ "redefinition of " ++ s
    Nothing -> (newDataDefs, newFnDefs)
  where
    dataNames = map (\(DataDef n _ _) -> n) dataDefs
    dataConstrs = map fst $ concatMap (\(DataDef _ _ cs) -> cs) dataDefs
    fnNames = map (\(FnDef n _ _) -> n) fnDefs
    initEnv = snd (mapAccumL addBind primEnv (dataConstrs ++ fnNames))
    newDataDefs = map (renameData initEnv) dataDefs
    newFnDefs = map (renameFn initEnv) fnDefs
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Just a  <|> _ = Just a
    Nothing <|> b = b
    redefName =
      checkUnique dataNames <|> checkUnique dataConstrs <|> checkUnique fnNames

renameData :: RenameEnv -> DataDef RdrName -> DataDef Name
renameData initEnv (DataDef name tps constrs) =
  DataDef name tps (map renameConstr constrs)
  where
    renameConstr (constr, tsig) =
      (((! constr) . fst) initEnv, tsig)

renameFn :: RenameEnv -> FnDef RdrName -> FnDef Name
renameFn initEnv (FnDef fName params body) =
  FnDef newFName newParams newBody
  where
    newFName = ((! fName) . fst) initEnv
    (newParams, env) = mapAccumL addBind initEnv params
    newBody = renameExpr env body

renameExpr :: RenameEnv -> Expr RdrName -> Expr Name
renameExpr _ (IntE n) = IntE n
renameExpr (nmap, _) (VarE s) =
  case mLookup s nmap of
    Just name -> VarE name
    Nothing -> error $ "undefined name " ++ s
renameExpr env (AppE e1 e2) =
  AppE (renameE e1) (renameE e2)
  where
    renameE = renameExpr env
renameExpr env (CaseE e brs) =
  CaseE (renameE e) (map renameBr brs)
  where
    renameE = renameExpr env
    renameBr (cons, bindNames, body) =
      (newCons, newBindNames, newBody)
      where
        newCons = ((! cons) . fst) env
        (newBindNames, newEnv) = mapAccumL addBind env bindNames
        newBody = renameExpr newEnv body
renameExpr env (LetE binds e) =
  LetE newBinds (renameE e)
  where
    (bindNames, bindEs) = unzip binds
    (newBindNames, newEnv) = mapAccumL addBind env bindNames
    newBinds = zip newBindNames (map renameE bindEs)
    renameE = renameExpr newEnv
