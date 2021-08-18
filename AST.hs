module AST where

data TypeSig
  = AtomTS String
  | ArrowTS TypeSig TypeSig
  deriving (Show)

type TypeDecl = (String, TypeSig)

data Expr
  = VarE String 
  | ILitE Int 
  | ApE Expr Expr
  | CaseE Expr [Branch]
  deriving (Show)

type Pattern = [String]
type Branch = (Pattern, Expr)
type Constructor = (String, [TypeSig])

type FnDef = (String, [String], Expr)
type DataDef = (String, [Constructor])

data Statement
  = FnDSTMT FnDef
  | DataDSTMT DataDef
  | DeclSTMT TypeDecl
  deriving (Show)

type Program = [Statement]
