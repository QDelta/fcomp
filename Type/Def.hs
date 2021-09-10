module Type.Def where

import Utils.Set

data MType
  = IntT
  | VarT Int
  | DataT String
  | ArrT MType MType
  deriving (Eq)

data PType = Forall (Set Int) MType

instance (Show MType) where
  show IntT = "Int"
  show (VarT n) = show n
  show (DataT n) = n
  show (ArrT t1 t2) = showParen t1 ++ " -> " ++ show t2
    where
      showParen t@(ArrT _ _) = "(" ++ show t ++ ")"
      showParen t = show t


