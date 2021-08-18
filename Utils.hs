module Utils where

type Map k v = [(k, v)]

mLookUp :: Eq k => Map k v -> k -> v
mLookUp [] _ = error "Map: nonexistent key"
mLookUp ((k0, v0) : m')  k 
  | k == k0 = v0
  | otherwise = mLookUp m' k

mLookUpMaybe :: Eq k => Map k v -> k -> Maybe v
mLookUpMaybe [] _ = Nothing 
mLookUpMaybe ((k0, v0) : m')  k 
  | k == k0 = Just v0
  | otherwise = mLookUpMaybe m' k

mInsert :: Map k v -> (k, v) -> Map k v
mInsert = flip (:)

mFromList :: [(k, v)] -> Map k v
mFromList l = l

mDrop :: Int -> Map k v -> Map k v
mDrop = drop

emptyMap :: Map k v
emptyMap = []

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)