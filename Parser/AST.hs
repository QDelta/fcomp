module Parser.AST where

data TypeSig
  = AtomTS String
  | ArrTS TypeSig TypeSig
  deriving (Show)

data Expr
  = IntE Int
  | VarE String
  | AppE Expr Expr
  | CaseE Expr [Branch]
  deriving (Show)

type Branch = ([String], Expr) -- pattern, body
type Constructor = (String, [TypeSig]) -- name, types

data DataDef = DataDef String [Constructor] -- name, constructors
  deriving (Show)
data FnDef = FnDef String [String] Expr -- name, params, body
  deriving (Show)

type Program = ([DataDef], [FnDef])
