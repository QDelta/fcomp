{-# LANGUAGE LambdaCase #-}

module Parser.Parser (parse) where

import Utils
import Parser.AST
import Parser.Parsec
import Data.Char (ord)

data Token 
  = LParen | RParen
  | DefKW  | DataKW | CaseKW
  | Arrow
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
    | c == '\'' -> let (c, rest) = charLex   cs in c : tlex rest
    | isDigit c -> let (n, rest) = intLex  c cs in n : tlex rest
    | otherwise -> let (n, rest) = nameLex c cs in n : tlex rest

isSpace :: Char -> Bool
isSpace = (`elem` " \t\r\n")

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c || c `elem` "()")

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

type TParser a = Parser Token a

pName :: TParser String
pName = Parser $ \case
  NameTok n : ts -> Just (n, ts)
  _ -> Nothing

pInt :: TParser Int
pInt = Parser $ \case
  IntTok n : rest -> Just (n, rest)
  _ -> Nothing

pChar :: TParser Char
pChar = Parser $ \case
  CharTok c : rest -> Just (c, rest)
  _ -> Nothing

pparened :: TParser a -> TParser a
pparened p = do
  _ <- psym LParen
  a <- p
  _ <- psym RParen
  return a

pProgram :: TParser Program 
pProgram = pplus pDef

pDef :: TParser Definition 
pDef = pparened $
  (psym DataKW >> pData) <|>
  (psym DefKW >> pFn)

pData :: TParser Definition
pData = do
  n <- pName
  cs <- pplus pConstructor
  return (DataDef n cs)

pConstructor :: TParser Constructor
pConstructor = 
  (do
    n <- pName
    return (n, [])
  ) <|> pparened
  (do
    n <- pName
    ts <- pplus pTypeSig
    return (n, ts)
  )

pTypeSig :: TParser TypeSig
pTypeSig =
  AtomTS <$> pName
  <|>
  (do
    _ <- psym Arrow
    ts <- pplus pTypeSig
    return $foldr1 ArrTS ts
  )

pFn :: TParser Definition
pFn = do
  name : params <- pNameList
  FnDef name params <$> pExpr

pNameList :: TParser [String]
pNameList =
  (do
    n <- pName
    return [n]
  ) <|> pparened (pplus pName)

pExpr :: TParser Expr
pExpr = 
  IntE <$> pInt <|>
  IntE . ord <$> pChar <|>
  VarE <$> pName 
  <|> pparened
  (do
    _ <- psym CaseKW
    e <- pExpr
    brs <- pplus pBranch
    return (CaseE e brs)
  ) <|> pparened
  (do
    es <- pplus pExpr
    return (foldl1 AppE es)
  )

pBranch :: TParser Branch 
pBranch = pparened (pseq pNameList pExpr)

parse :: String -> Program
parse s = case pp (tlex s) of
  Just (p, []) -> p
  _ -> error "ParseError"
  where Parser pp = pProgram
