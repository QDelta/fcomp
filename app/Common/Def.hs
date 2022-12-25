module Common.Def where

type RdrName = String
type Ident = Int
newtype Name = Name (String, Ident)       -- original name, identifier

getRdrName :: Name -> String
getRdrName (Name (s, _)) = s

getIdent :: Name -> Ident
getIdent (Name (_, n)) = n

instance Eq Name where
  Name (_, a) == Name (_, b) = a == b

instance Ord Name where
  compare (Name (_, a)) (Name (_, b)) = compare a b

instance Show Name where
  show (Name (s, i)) = s ++ '#' : show i