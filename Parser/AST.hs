module Parser.AST where

data TypeSig
  = VarTS String
  | DataTS String [TypeSig]
  | ArrTS TypeSig TypeSig
  deriving (Show)

data Expr
  = IntE Int
  | VarE String
  | AppE Expr Expr
  | CaseE Expr [Branch]
  deriving (Show)

type Branch = (String, [String], Expr) -- constructor, bindings, body

type Constructor = (String, [TypeSig]) -- name, types

data DataDef = 
  DataDef String [String] [Constructor] -- name, type params, constructors
  deriving (Show)

data FnDef = 
  FnDef String [String] Expr -- name, params, body
  deriving (Show)

type Program = ([DataDef], [FnDef])
