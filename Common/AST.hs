module Common.AST where

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
