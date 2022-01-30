module Utils.Set
  ( Set,
    emptySet,
    sElem,
    sInsert,
    sRemove,
    sToList,
    sFromList,
    sUnion,
    sSingleton,
    sIsEmpty
  ) where

import qualified Data.Set as Set

newtype Set k = S (Set.Set k)

emptySet :: Set k
emptySet = S Set.empty

sElem :: Ord k => k -> Set k -> Bool
sElem k (S s) = Set.member k s

sInsert :: Ord k => Set k -> k -> Set k
sInsert (S s) k = S (Set.insert k s)

sRemove :: Ord k => Set k -> k -> Set k
sRemove (S s) k = S (Set.delete k s)

sToList :: Ord k => Set k -> [k]
sToList (S s) = Set.toList s

sFromList :: Ord k => [k] -> Set k
sFromList = S . Set.fromList

sUnion :: Ord k => Set k -> Set k -> Set k
sUnion (S s1) (S s2) = S (Set.union s1 s2)

sSingleton :: Ord k => k -> Set k
sSingleton = S . Set.singleton

sIsEmpty :: Set k -> Bool
sIsEmpty (S s) = Set.null s

instance Foldable Set where
  foldl f x (S s) = foldl f x s
  foldr f x (S s) = foldr f x s

instance Show k => Show (Set k) where
  show (S s) = show s
