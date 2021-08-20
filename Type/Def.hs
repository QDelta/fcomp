module Type.Def where

data Type
  = DataT String
  | FnT Type Type
  deriving (Show, Eq)
