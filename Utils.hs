module Utils where

import Data.Maybe (fromMaybe)

sElem :: Eq k => k -> [k] -> Bool
sElem _ [] = False
sElem k (k0 : s') = k == k0 || k `sElem` s'

sRemove :: Eq k => [k] -> k -> [k]
sRemove [] _ = []
sRemove (k0 : s') k
  | k == k0 = s'
  | otherwise = k0 : sRemove s' k

mLookupMaybe :: Eq k => [(k, v)] -> k -> Maybe v
mLookupMaybe [] _ = Nothing
mLookupMaybe ((k0, v0) : m') k
  | k == k0 = Just v0
  | otherwise = mLookupMaybe m' k

-- lookup with a default value
mLookup :: Eq k => [(k, v)] -> k -> v -> v
mLookup m k defalt = fromMaybe defalt (mLookupMaybe m k)

mInsert :: [(k, v)] -> (k, v) -> [(k, v)]
mInsert = flip (:)

mElem :: Eq k => k -> [(k, v)] -> Bool
mElem k m = sElem k (map fst m)

mShow :: (Show k, Show v) => [(k, v)] -> String
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

-- insertion sort
isortBy :: Ord k => (a -> k) -> [a] -> [a]
isortBy _ [] = []
isortBy f (x : xs) = ins x (isortBy f xs)
  where
    ins x [] = [x]
    ins x (y : ys)
      | f x < f y = x : y : ys
      | otherwise = y : ins x ys

isort :: Ord k => [k] -> [k]
isort = isortBy id

-- genUnique :: (Eq k, Ord k) => [k] -> k -> k
-- genUnique s def = genUniqueFrom sorted (minOrDef sorted def)
--   where
--     sorted = sort s
--     genUniqueFrom [] k = k
--     genUniqueFrom (k0 : ks) k
--       | k == k0 = genUniqueFrom ks (succ k)
--       | otherwise = k

checkUnique :: (Ord k) => [k] -> Bool
checkUnique = checkUSorted . isort
  where
    checkUSorted [] = True
    checkUSorted [x] = True
    checkUSorted (x : y : l) = x /= y && checkUSorted (y : l)