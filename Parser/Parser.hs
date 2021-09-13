{-# LANGUAGE LambdaCase #-}

module Parser.Parser (parse) where

import Utils.Function
import Parser.AST
import Parser.Parsec

data Token 
  = LParen | RParen
  | LBrace | RBrace
  | DataKW | CaseKW | OfKW
  | Arrow  | SemiC  | Equal | Or
  | LineComment
  | NameTok String
  | IntTok Int
  deriving (Show, Eq)

tlex :: String -> [Token]
tlex s = case s of
  [] -> []
  c : cs
    | isSpace c -> tlex cs
    | isDigit c -> callLex intLex
    | isAlpha c -> callLex nameLex
    | otherwise -> callLex symLex
  where
    callLex f = toks
      where 
        (tok, rest) = f s
        toks =
          if tok == LineComment then
            tlex (skipUntil '\n' rest)
          else
            tok : tlex rest

skipUntil :: Char -> String -> String
skipUntil c [] = []
skipUntil c (h : t)
  | h == c    = t
  | otherwise = skipUntil c t

isSpace :: Char -> Bool
isSpace = (`elem` " \t\r\n")

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

isAlpha :: Char -> Bool
isAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

intLex :: String -> (Token, String)
intLex s = (ti, rest)
  where
    (istr, rest) = span isDigit s
    ti = IntTok (read istr)

nameLex :: String -> (Token, String)
nameLex s = (tn, rest)
  where 
    isNameC c = isAlpha c || isDigit c
    (name, rest) = span isNameC s
    tn = case name of
      "data" -> DataKW
      "case" -> CaseKW
      "of"   -> OfKW
      _      -> NameTok name

symLex :: String -> (Token, String)
symLex s = symLexFrom s symbols
  where
    symLexFrom _ [] = error "Lex error"
    symLexFrom s ((sym, tok) : ss) =
      case splitPrefix s sym of
        Just rest -> (tok, rest)
        Nothing -> symLexFrom s ss

splitPrefix :: String -> String -> Maybe String
splitPrefix s [] = Just s
splitPrefix (c : s) (p : ps)
  | c == p    = splitPrefix s ps
  | otherwise = Nothing

symbols :: [(String, Token)]
symbols =
  [ ("(",  LParen),
    (")",  RParen),
    ("{",  LBrace),
    ("}",  RBrace),
    ("->", Arrow ),
    ("|",  Or    ),
    (";",  SemiC ),
    ("=",  Equal ),
    ("--", LineComment)
  ]

type TParser a = Parser Token a

pName :: TParser String
pName = Parser $ \case
  NameTok n : ts -> Just (n, ts)
  _ -> Nothing

pInt :: TParser Int
pInt = Parser $ \case
  IntTok n : rest -> Just (n, rest)
  _ -> Nothing

pparened :: TParser a -> TParser a
pparened p = do
  psym LParen
  a <- p
  psym RParen
  return a

pbraced :: TParser a -> TParser a
pbraced p = do
  psym LBrace
  a <- p
  psym RBrace
  return a

pSemiC :: TParser Token
pSemiC = psym SemiC

type Definition = Either DataDef FnDef
type PreProgram = [Definition]

pProgram :: TParser PreProgram 
pProgram = pplus pDef

pDef :: TParser Definition 
pDef = 
  (Left <$> pData) <|>
  (Right <$> pFn)

pData :: TParser DataDef
pData = do
  psym DataKW
  n <- pName
  psym Equal
  cs <- pinterleave pConstructor (psym Or)
  pSemiC
  return (DataDef n cs)

pConstructor :: TParser Constructor
pConstructor = do
  n <- pName
  ts <- pstar pTypeSig
  return (n, ts)

pTypeSig :: TParser TypeSig
pTypeSig =
  AtomTS <$> pName
  <|> pparened 
  (do
    ts <- pinterleave pTypeSig (psym Arrow)
    return (foldr1 ArrTS ts)
  )

pFn :: TParser FnDef
pFn = do
  name <- pName
  params <- pstar pName
  psym Equal
  body <- pExpr
  psym SemiC
  return (FnDef name params body)

pExpr :: TParser Expr
pExpr = 
  (do
    psym CaseKW
    e <- pExpr
    psym OfKW
    brs <- pbraced $ pinterleave pBranch pSemiC
    return (CaseE e brs)
  ) <|>
  (do
    es <- pplus pExprAtom
    return (foldl1 AppE es)
  )

pExprAtom :: TParser Expr
pExprAtom =
  IntE <$> pInt <|>
  VarE <$> pName <|>
  pparened pExpr

pBranch :: TParser Branch 
pBranch = do
  pat <- pplus pName
  psym Arrow
  expr <- pExpr
  return (pat, expr)

parse :: String -> Program
parse s = case pp (tlex s) of
  Just (p, []) -> reorder p
  _ -> error "ParseError"
  where 
    Parser pp = pProgram
    reorder [] = ([], [])
    reorder (Left  dDef : rest) = first  (dDef :) $ reorder rest
    reorder (Right fDef : rest) = second (fDef :) $ reorder rest
