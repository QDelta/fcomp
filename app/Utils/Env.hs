module Utils.Env
  ( Env, empty, member, lookup, (!)
  , bind, delete, fromList
  ) where

-- multimap with name shadowing

import Prelude hiding (head, tail, lookup)
import Data.List.NonEmpty hiding (fromList)
import qualified Data.Map as M

newtype Env k v = Env (M.Map k (NonEmpty v))

empty :: Env k v
empty = Env M.empty

member :: Ord k => k -> Env k v -> Bool
member k (Env m) =
  case M.lookup k m of
    Nothing -> False
    Just _ -> True

lookup :: Ord k => k -> Env k v -> Maybe v
lookup k (Env m) =
  case M.lookup k m of
    Nothing -> Nothing
    Just (v :| _) -> Just v

(!) :: Ord k => Env k v -> k -> v
Env m ! k = head (m M.! k)

bind :: Ord k => (k, v) -> Env k v -> Env k v
bind (k, v) (Env m) = Env $ M.insertWith append k (v :| []) m

delete :: Ord k => k -> Env k v -> Env k v
delete k (Env m) = Env $ M.update maybeTail k m
  where
    maybeTail l = case tail l of
      []    -> Nothing
      h : t -> Just (h :| t)

fromList :: Ord k => [(k, v)] -> Env k v
fromList = foldr bind empty