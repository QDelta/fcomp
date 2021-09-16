module Utils.Function where

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

assertJust :: Maybe t -> t
assertJust (Just a) = a

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
isortBy :: (a -> a -> Ordering) -> [a] -> [a]
isortBy _ [] = []
isortBy cmp (x : xs) = ins x (isortBy cmp xs)
  where
    ins x [] = [x]
    ins x (y : ys)
      | cmp x y == GT = y : ins x ys
      | otherwise     = x : y : ys

isortWith :: Ord k => (a -> k) -> [a] -> [a]
isortWith f = isortBy (\x y -> compare (f x) (f y))

isort :: Ord k => [k] -> [k]
isort = isortBy compare

checkUnique :: (Ord k) => [k] -> Bool
checkUnique = checkUSorted . isort
  where
    checkUSorted [] = True
    checkUSorted [x] = True
    checkUSorted (x : y : l) = x /= y && checkUSorted (y : l)

classify :: (a -> Bool) -> [a] -> ([a], [a])
classify _ [] = ([], [])
classify f (x : xs)
  | f x       = first  (x :) rest
  | otherwise = second (x :) rest
  where rest = classify f xs
