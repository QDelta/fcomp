{-# LANGUAGE LambdaCase #-}

module Parser.Parser (parse) where

import Utils
import Parser.AST
import Data.Char (ord)
import Control.Applicative

data Token 
  = LParen | RParen
  | DefKW  | DataKW | CaseKW
  | Colon  | Arrow
  | NameTok String
  | IntTok Int
  | CharTok Char
  deriving (Show, Eq)

tlex :: String -> [Token]
tlex s = case s of
  [] -> []
  c : cs
    | isSpace c -> tlex cs
    | c == '('  -> LParen : tlex cs
    | c == ')'  -> RParen : tlex cs
    | c == ':'  -> Colon  : tlex cs
    | c == '\'' -> let (c, rest) = charLex   cs in c : tlex rest
    | isDigit c -> let (n, rest) = intLex  c cs in n : tlex rest
    | otherwise -> let (n, rest) = nameLex c cs in n : tlex rest

isSpace :: Char -> Bool
isSpace = (`elem` " \t\r\n")

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c || c `elem` ":()#")

intLex :: Char -> String -> (Token, String)
intLex c cs = (ti, rest)
  where
    istr = c : istr'
    (istr', rest) = span isDigit cs
    ti = IntTok (read istr)

nameLex :: Char -> String -> (Token, String)
nameLex c cs = (tn, rest)
  where 
    n = c : name'
    (name', rest) = span isNameChar cs
    tn = case n of
      "def"  -> DefKW
      "data" -> DataKW
      "case" -> CaseKW
      "->"   -> Arrow
      _      -> NameTok n

charLex :: String -> (Token, String)
charLex (c : '\'' : rest) = (CharTok c, rest)
charLex _ = error "Invalid character literal"

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

pleft :: Parser i a -> Parser i b -> Parser i a
pleft pa pb = fst <$> pseq pa pb

pright :: Parser i a -> Parser i b -> Parser i b
pright pa pb = snd <$> pseq pa pb

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

type TParser a = Parser Token a

pLParen :: TParser Token
pLParen = psym LParen

pRParen :: TParser Token
pRParen = psym RParen

pparened :: TParser a -> TParser a
pparened p = do
  _ <- pLParen
  a <- p
  _ <- pRParen
  return a

pName :: TParser String
pName = Parser $ \case
  NameTok n : ts -> Just (n, ts)
  _ -> Nothing

pProgram :: TParser Program 
pProgram = pplus pStmt

pStmt :: TParser Statement
pStmt = pparened $
  pright (psym DataKW) (DataDSTMT <$> pData) <|>
  pright (psym DefKW) (FnDSTMT <$> pFn) <|>
  pright (psym Colon) (DeclSTMT <$> pTypeDecl)

pData :: TParser DataDef
pData = do
  n <- pName
  cs <- pplus pConstructor
  return (n, cs)

pConstructor :: TParser Constructor
pConstructor = 
  (do
    n <- pName
    return (n, []))
  <|> pparened
  (do
    n <- pName
    ts <- pplus pTypeSig
    return (n, ts))

pTypeSig :: TParser TypeSig
pTypeSig =
  AtomTS <$> pName
  <|> pparened
  (do
    _ <- psym Arrow
    ts <- pplus pTypeSig
    return (foldl1 ArrowTS ts))

pFn :: TParser FnDef 
pFn = do
  name : params <- pPattern
  body <- pExpr
  return (name, params, body)

pPattern :: TParser Pattern
pPattern =
  (do
    n <- pName
    return [n])
  <|> pparened (pplus pName)

pExpr :: TParser Expr
pExpr = 
  IntLitE <$> pInt <|>
  IntLitE . ord <$> pChar <|>
  VarE <$> pName 
  <|> pparened
  (do
    _ <- psym CaseKW
    e <- pExpr
    brs <- pplus pBranch
    return (CaseE e brs))
  <|> pparened
  (do
    es <- pplus pExpr
    return (foldl1 ApE es))

pInt :: TParser Int
pInt = Parser $ \case
  IntTok n : rest -> Just (n, rest)
  _ -> Nothing

pChar :: TParser Char
pChar = Parser $ \case
  CharTok c : rest -> Just (c, rest)
  _ -> Nothing

pBranch :: TParser Branch 
pBranch = pparened (pseq pPattern pExpr)

pTypeDecl :: TParser TypeDecl
pTypeDecl = pseq pName pTypeSig

parse :: String -> Program
parse s = case pp (tlex s) of
  Just (p, []) -> p
  _ -> error "ParseError"
  where Parser pp = pProgram
