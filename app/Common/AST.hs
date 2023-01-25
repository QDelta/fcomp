module Common.AST where

import Common.Def

data Expr v
  = IntE Int
  | VarE v
  | AppE (Expr v) (Expr v)
  | CaseE (Expr v) [Branch v]
  | LetE (Bind v) (Expr v)
  | LetRecE [Bind v] (Expr v)
  | LambdaE [v] (Expr v)
  deriving (Show)

data TypeSig
  = VarTS RdrName
  | DataTS RdrName [TypeSig]
  | ArrTS TypeSig TypeSig
  deriving (Show)

type Bind v = (v, Expr v)                 -- name, expression
type Branch v = (v, [v], Expr v)          -- constructor, bindings, body

type Constructor v = (v, [TypeSig])       -- name, types

type DataBind v =
  (RdrName, [RdrName], [Constructor v])   -- name, type params, constructors

newtype DataGroup v
  = RecData [DataBind v]
  deriving (Show)

data ValGroup v
  = ValDef (Bind v)
  | RecVal [Bind v]
  deriving (Show)

type Program v = ([DataGroup v], [ValGroup v])

getValBinds :: ValGroup v -> [Bind v]
getValBinds (ValDef val) = [val]
getValBinds (RecVal vals) = vals

getValNames :: ValGroup v -> [v]
getValNames (ValDef val) = [fst val]
getValNames (RecVal vals) = map fst vals

getDataNames :: DataGroup v -> [RdrName]
getDataNames (RecData datas) = map (\(a, b, c) -> a) datas

getConstrNames :: DataGroup v -> [v]
getConstrNames (RecData datas) = map fst (concatMap (\(a, b, c) -> c) datas)