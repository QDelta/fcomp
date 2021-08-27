module Utils where

type Map k v = [(k, v)]

mLookup :: Eq k => Map k v -> k -> v
mLookup [] _ = error "Map: nonexistent key"
mLookup ((k0, v0) : m') k 
  | k == k0 = v0
  | otherwise = mLookup m' k

mLookupMaybe :: Eq k => Map k v -> k -> Maybe v
mLookupMaybe [] _ = Nothing 
mLookupMaybe ((k0, v0) : m') k 
  | k == k0 = Just v0
  | otherwise = mLookupMaybe m' k

mInsert :: Map k v -> (k, v) -> Map k v
mInsert = flip (:)

mFromList :: [(k, v)] -> Map k v
mFromList l = l

mToList :: Map k v -> [(k, v)]
mToList l = l

mElem :: Eq k => k -> Map k v -> Bool
mElem _ [] = False
mElem k ((k0, _) : m')
  | k == k0 = True
  | otherwise = mElem k m'

mCount :: Map k v -> Int 
mCount = length

emptyMap :: Map k v
emptyMap = []

mShow :: (Show k, Show v) => Map k v -> String
mShow = concat . interleave "\n" . map show

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

mapAccumL :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f acc [] = (acc, [])
mapAccumL f acc (x : xs) = (acc2, x' : xs')
  where
    (acc1, x') = f acc x
    (acc2, xs') = mapAccumL f acc1 xs

interleave :: a -> [a] -> [a]
interleave _ [x] = [x]
interleave x (x1 : xs) = x1 : x : interleave x xs
