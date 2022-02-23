module Utils.Env
  () where

-- multimap with name shadowing

import Utils.Map

newtype Env k v = Env (Map k [v])

emptyEnv :: Env k v
emptyEnv = Env emptyMap

eElem :: Ord k => k -> Env k v -> Bool
eElem k (Env m) =
  case mLookup k m of
    Nothing -> False
    Just [] -> False
    Just _  -> True

eLookup :: Ord k => k -> Env k v -> Maybe v
eLookup k (Env m) =
  case mLookup k m of
    Nothing -> Nothing
    Just [] -> Nothing
    Just (v : _) -> Just v

(!) :: Ord k => Env k v -> k -> v
(Env m) ! k = head (m Utils.Map.! k)

eBind :: Ord k => (k, v) -> Env k v -> Env k v
eBind (k, v) (Env m) = Env $ mUpdate (Just . (v :)) k m

eRemove :: Ord k => k -> Env k v -> Env k v
eRemove k (Env m) = Env $ mUpdate maybeTail k m
  where
    maybeTail [] = Nothing
    maybeTail (h : t) = Just t

