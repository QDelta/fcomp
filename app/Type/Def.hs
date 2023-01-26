module Type.Def
  ( MType(..), PType(..)
  ) where

data MType
  = VarT Int
  | DataT String [MType]
  | ArrT MType MType
  deriving (Eq)

data PType = Forall [Int] MType

data TyPrec = Open | Arrow | App

showPrec :: TyPrec -> MType -> String
showPrec _  (VarT n) = 't' : show n
showPrec pr (DataT n ts) = showData pr n ts
showPrec pr (ArrT l r) = showArr pr l r

showData :: TyPrec -> String -> [MType] -> String
showData pr n ts =
  case pr of
    App | not (null ts) -> "(" ++ s ++ ")"
    _ -> s
  where
    s = unwords (n : map (showPrec App) ts)

showArr :: TyPrec -> MType -> MType -> String
showArr pr l r =
  case pr of
    Open -> s
    _   -> "(" ++ s ++ ")"
  where
    s = showPrec Arrow l ++ " -> " ++ showPrec Open r

instance Show MType where
  show = showPrec Open

instance Show PType where
  show (Forall ps t) =
    (if null ps then "" else tparamStr) ++ show t
    where
      tparamStr = "forall" ++ concatMap (\p -> " t" ++ show p) ps ++ ". "