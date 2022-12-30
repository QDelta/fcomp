module Type.Def
  ( MType(..), PType(..)
  , DataAttr
  ) where

import Utils.Set

data MType
  = VarT Int
  | DataT String [MType]
  | ArrT MType MType
  deriving (Eq)

data PType = Forall (Set Int) MType

type DataAttr = (Int, Bool) -- number of type parameters, is number type
-- data type with only constant constructors can be translate to integers

data TyPrec = Open | Arrow | App

showPrec :: TyPrec -> MType -> String
showPrec pr t = case t of
  VarT n     -> 't' : show n
  DataT n ts -> showData pr n ts
  ArrT l r   -> showArr pr l r

showData :: TyPrec -> String -> [MType] -> String
showData pr n ts = case pr of
  App | not (null ts) -> "(" ++ s ++ ")"
  _ -> s
  where
    s = unwords (n : map (showPrec App) ts)

showArr :: TyPrec -> MType -> MType -> String
showArr pr l r = case pr of
  Open -> s
  _   -> "(" ++ s ++ ")"
  where
    s = showPrec Arrow l ++ " -> " ++ showPrec Open r

instance Show MType where
  show = showPrec Open

instance Show PType where
  show (Forall ps t) =
    (if sIsEmpty ps then "" else tparamStr) ++ show t
    where
      tparamStr =
        "forall" ++ concatMap (\p -> " t" ++ show p) (sToList ps) ++ ". "