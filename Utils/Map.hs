module Utils.Map
  ( Map
  , emptyMap
  , mElem
  , mLookup
  , (!)
  , mInsert
  , mUpdate
  , mRemove
  , mToList
  , mFromList
  , mSize
  ) where

import Utils.Function
import qualified Data.Map as Map

newtype Map k v = M (Map.Map k v)

emptyMap :: Map k v
emptyMap = M Map.empty

mElem :: Ord k => k -> Map k v -> Bool
mElem k (M m) = Map.member k m

mLookup :: Ord k => k -> Map k v -> Maybe v
mLookup k (M m) = Map.lookup k m

(!) :: Ord k => Map k v -> k -> v
M m ! k = m Map.! k

mInsert :: Ord k => (k, v) -> Map k v -> Map k v
mInsert (k, v) (M m) = M (Map.insert k v m)

mUpdate :: Ord k => (v -> v) -> k -> Map k v -> Map k v
mUpdate f k (M m) = M (Map.update (Just . f) k m)

mRemove :: Ord k => k -> Map k v -> Map k v
mRemove k (M m) = M (Map.delete k m)

mToList :: Ord k => Map k v -> [(k, v)]
mToList (M m) = Map.toList m

mFromList :: Ord k => [(k, v)] -> Map k v
mFromList l = M (Map.fromList l)

mSize :: Ord k => Map k v -> Int
mSize (M m) = Map.size m

instance (Ord k, Show k, Show v) => Show (Map k v) where
  show m = concat $ interleave "\n" (map show (mToList m))
