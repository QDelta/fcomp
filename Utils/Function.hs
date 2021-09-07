module Utils.Function where

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

mapAccumL :: (a -> b -> (c, a)) -> a -> [b] -> ([c], a)
mapAccumL f acc [] = ([], acc)
mapAccumL f acc (x : xs) = (x' : xs', acc2)
  where
    (x', acc1) = f acc x
    (xs', acc2) = mapAccumL f acc1 xs

interleave :: a -> [a] -> [a]
interleave _ [] = []
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