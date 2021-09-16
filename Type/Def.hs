module Type.Def where

import Utils.Set

data MType
  = VarT Int
  | DataT String [MType]
  | ArrT MType MType
  deriving (Eq)

data PType = Forall (Set Int) MType

instance (Show MType) where
  show t = case t of
    VarT n -> show n
    DataT n ts -> n ++ concatMap (\t -> ' ' : showP t) ts
    ArrT l r -> showP l ++ " -> " ++ show r
    where
      showP t@(ArrT _ _) = '(' : show t ++ ")"
      showP t = show t

instance (Show PType) where
  show (Forall ps t) =
    (if sIsEmpty ps then "" else tparamStr) ++ show t
    where 
      showp p = ' ' : show p
      tparamStr = "forall" ++ concatMap showp ps ++ ". "
