module Utils.Env
  ( Env
  , emptyEnv
  , eElem
  , eLookup
  , (!)
  , eBind
  , eRemove
  , eFromList
  ) where

-- multimap with name shadowing

import Prelude hiding (head, tail)
import Data.List.NonEmpty
import qualified Utils.Map as M

newtype Env k v = Env (M.Map k (NonEmpty v))

emptyEnv :: Env k v
emptyEnv = Env M.emptyMap

eElem :: Ord k => k -> Env k v -> Bool
eElem k (Env m) = case M.mLookup k m of
  Nothing -> False
  Just _  -> True

eLookup :: Ord k => k -> Env k v -> Maybe v
eLookup k (Env m) =
  case M.mLookup k m of
    Nothing -> Nothing
    Just (v :| _) -> Just v

(!) :: Ord k => Env k v -> k -> v
Env m ! k = head (m M.! k)

eBind :: Ord k => (k, v) -> Env k v -> Env k v
eBind (k, v) (Env m) = Env $ M.mInsertWith append (k, v :| []) m

eRemove :: Ord k => k -> Env k v -> Env k v
eRemove k (Env m) = Env $ M.mUpdate maybeTail k m
  where
    maybeTail l = case tail l of
      []    -> Nothing
      h : t -> Just (h :| t)

eFromList :: Ord k => [(k, v)] -> Env k v
eFromList = foldr eBind emptyEnv