{-# LANGUAGE TupleSections #-}

module Utils.State
  ( State(..),
    execState,
    evalState
  ) where

import Utils.Function

newtype State s a = State { runState :: s -> (a, s) }

execState :: State s a -> s -> s
execState s = snd . runState s

evalState :: State s a -> s -> a
evalState s = fst . runState s

instance Monad (State s) where
  return x = State (x,)
  (State r1) >>= f = State $
    \s1 -> let
      (x, s2) = r1 s1
      State r2 = f x
    in r2 s2

instance Functor (State s) where
  fmap f (State r) = State (first f . r)

instance Applicative (State s) where
  pure = return
  sab <*> sa = do
    fab <- sab
    fab <$> sa
