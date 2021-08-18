module AST where

data Expr
  = VarE String 
  | ILitE Int 
  | ApE Expr Expr
  | CaseE Expr [Branch]
  deriving (Show)

data Pattern
  = VarP String
  | ConstrP String [String]
  deriving (Show)

type Branch = (Pattern, Expr)
type TypeSig = [String]
type Parameter = (String, TypeSig)
type Constructor = (String, [TypeSig])

data Definition
  = FnDef String [Parameter] Expr
  | DataDef String [Constructor]
  deriving (Show)

type Program = [Definition]
