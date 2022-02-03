module Parser.Lexer (tlex) where

import Parser.Parser (Token(..))

tlex :: String -> [Token]
tlex s = case s of
  [] -> []
  c : cs
    | isSpace c -> tlex cs
    | isDigit c -> callLex intLex
    | isUpperAlpha c -> callLex (nameLex UNameTok)
    | isLowerAlpha c -> callLex (nameLex LNameTok)
    | otherwise -> callLex symLex
  where
    callLex :: (String -> (Token, String)) -> [Token]
    callLex lexer = toks
      where
        (tok, rest) = lexer s
        toks =
          if tok == LineComment then
            tlex (skipLine rest)
          else
            tok : tlex rest
    skipLine :: String -> String
    skipLine [] = []
    skipLine (c : s)
      | c == '\n' = s
      | otherwise = skipLine s

isSpace :: Char -> Bool
isSpace = (`elem` " \t\r\n")

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

isUpperAlpha :: Char -> Bool
isUpperAlpha c = 'A' <= c && c <= 'Z'

isLowerAlpha :: Char -> Bool
isLowerAlpha c = 'a' <= c && c <= 'z'

isAlpha :: Char -> Bool
isAlpha c = isUpperAlpha c || isLowerAlpha c

intLex :: String -> (Token, String)
intLex s = (ti, rest)
  where
    (istr, rest) = span isDigit s
    ti = IntTok (read istr)

nameLex :: (String -> Token) -> String -> (Token, String)
nameLex f s = (tn, rest)
  where
    isNameC c = isAlpha c || isDigit c
    (name, rest) = span isNameC s
    tn = case name of
      "data" -> DataKW
      "case" -> CaseKW
      "of"   -> OfKW
      "let"  -> LetKW
      "in"   -> InKW
      _      -> f name

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
splitPrefix [] (_ : _) = Nothing

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
