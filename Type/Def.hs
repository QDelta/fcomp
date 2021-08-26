module Type.Def where

data Type
  = DataT String
  | FnT Type Type
  | IntT
  deriving (Show, Eq)
