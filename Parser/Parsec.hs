{-# LANGUAGE LambdaCase #-}

module Parser.Parsec 
  ( Parser(..),
    (<|>),
    pitem,
    pseq,
    psat,
    psym,
    pstar,
    pplus,
    pinterleave
  ) where

import Utils.Function
import Control.Applicative (Alternative(..))

newtype Parser i o = Parser ([i] -> Maybe (o, [i]))

instance Monad (Parser i) where
  return a = Parser $ \is -> Just (a, is)
  (Parser pa) >>= f = Parser $ \is ->
    case pa is of
      Just (a, rest) -> pb rest
        where Parser pb = f a
      Nothing -> Nothing

instance Functor (Parser i) where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative (Parser i) where
  pure = return
  pab <*> pa = do
    fab <- pab
    fab <$> pa

instance Alternative (Parser i) where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \is ->
    case p1 is of
      r@(Just (_, _)) -> r
      Nothing -> p2 is

instance MonadFail (Parser i) where
  fail = error

pitem :: Parser i i
pitem = Parser $ \case
  t : ts -> Just (t, ts)
  [] -> Nothing

pseq :: Parser i a -> Parser i b -> Parser i (a, b)
pseq pa pb = do
  a <- pa
  b <- pb
  return (a, b)

psat :: (i -> Bool) -> Parser i i
psat f = do
  x <- pitem
  if f x then return x else empty

psym :: Eq i => i -> Parser i i
psym x = psat (== x)

pstar :: Parser i o -> Parser i [o]
pstar p = pplus p <|> return []

pplus :: Parser i o -> Parser i [o]
pplus p = do
  a <- p
  as <- pstar p
  return (a : as)

pinterleave :: Parser i a -> Parser i b -> Parser i [a]
pinterleave pa pb = do
  first <- pa
  rest <- pstar (snd <$> pseq pb pa)
  return (first : rest)