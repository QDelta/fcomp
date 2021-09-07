module Type.Def where

data Type
  = IntT
  | VarT Int
  | DataT String
  | ArrT Type Type
  deriving (Show, Eq)
