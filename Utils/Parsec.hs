{-# LANGUAGE LambdaCase #-}

module Utils.Parsec 
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

newtype Parser i o = Parser { runParser :: [i] -> Maybe (o, [i]) }

instance Monad (Parser i) where
  return = pure
  (Parser pa) >>= f = Parser $ \is ->
    case pa is of
      Just (a, rest) -> runParser (f a) rest
      Nothing -> Nothing

instance Functor (Parser i) where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative (Parser i) where
  pure a = Parser $ \is -> Just (a, is)
  pab <*> pa = do
    fab <- pab
    fab <$> pa

pempty :: Parser i o
pempty = Parser (const Nothing)

infixr 1 <|>

(<|>) :: Parser i o -> Parser i o -> Parser i o
Parser p1 <|> Parser p2 = Parser $ \is ->
  case p1 is of
    r@(Just _) -> r
    Nothing -> p2 is

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
  if f x then return x else pempty

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
