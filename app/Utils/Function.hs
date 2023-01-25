module Utils.Function where

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [x] = [x]
interleave x (x1 : xs) = x1 : x : interleave x xs

classify :: (a -> Bool) -> [a] -> ([a], [a])
classify _ [] = ([], [])
classify f (x : xs)
  | f x       = first  (x :) rest
  | otherwise = second (x :) rest
  where rest = classify f xs