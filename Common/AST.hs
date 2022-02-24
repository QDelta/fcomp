module Common.AST where

import Utils.Set
import Common.Def

data TypeSig
  = VarTS String
  | DataTS String [TypeSig]
  | ArrTS TypeSig TypeSig
  deriving (Show)

data Expr v
  = IntE Int
  | VarE v
  | AppE (Expr v) (Expr v)
  | CaseE (Expr v) [Branch v]
  | LetE [Bind v] (Expr v)
  | LambdaE [v] (Expr v)
  deriving (Show)

type Bind v = (v, Expr v)                 -- name, expression
type Branch v = (v, [v], Expr v)          -- constructor, bindings, body

type Constructor v = (v, [TypeSig])       -- name, types

data DataDef v =
  DataDef String [String] [Constructor v] -- name, type params, constructors
  deriving (Show)

data FnDef v =
  FnDef v [v] (Expr v)                    -- name, params, body
  deriving (Show)

type Program v = ([DataDef v], [FnDef v])

depsOfExprIn :: Set Ident -> Expr Name -> Set Ident
depsOfExprIn _ (IntE _) = emptySet
depsOfExprIn knowns (VarE v) =
  let id = getIdent v in
  if sElem id knowns then sSingleton id else emptySet
depsOfExprIn knowns (AppE e1 e2) =
  depOf e1 `sUnion` depOf e2
  where depOf = depsOfExprIn knowns
depsOfExprIn knowns (CaseE e brs) =
  foldl sUnion (depsOfExprIn knowns e) (map depsOfBr brs)
  where
    depsOfBr (_, brBinds, body) =
      depsOfExprIn newKnowns body
      where
        newKnowns = foldl sRemove knowns (map getIdent brBinds)
depsOfExprIn knowns (LetE binds e) =
  foldl sUnion (depsOf e) (map depsOf bindExprs)
  where
    (bindNames, bindExprs) = unzip binds
    bindIdents = map getIdent bindNames
    newKnowns = foldl sRemove knowns bindIdents
    depsOf = depsOfExprIn newKnowns
depsOfExprIn knowns (LambdaE params e) =
  depsOfExprIn newKnowns e
  where
    newKnowns = foldl sRemove knowns (map getIdent params)