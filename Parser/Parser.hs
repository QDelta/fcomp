module Parser.Parser where

import Parser.AST

data Token 
  = LParen | RParen
  | DefKW  | DataKW | CaseKW
  | Colon  | Arrow
  | NameTok String
  | IntTok Int
  deriving (Show, Eq)

tlex :: String -> [Token]
tlex s = case s of
  [] -> []
  c : cs
    | isSpace c -> tlex cs
    | c == '('  -> LParen : tlex cs
    | c == ')'  -> RParen : tlex cs
    | c == ':'  -> Colon  : tlex cs
    | isDigit c -> let (n, rest) = intLex  c cs in n : tlex rest
    | otherwise -> let (n, rest) = nameLex c cs in n : tlex rest

isSpace :: Char -> Bool
isSpace = (`elem` " \t\r\n")

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c || c `elem` ":()")

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

type Parser a = [Token] -> Maybe (a, [Token])

pseq :: Parser a -> Parser b -> Parser (a, b)
pseq pa pb ts = case pa ts of
  Just (ra, resta) -> case pb resta of
    Just (rb, rest) -> Just ((ra, rb), rest)
    Nothing -> Nothing 
  Nothing -> Nothing 

pmap :: (a -> b) -> Parser a -> Parser b
pmap f pa ts = case pa ts of
  Just (r, rest) -> Just (f r, rest)
  Nothing -> Nothing

pright :: Parser a -> Parser b -> Parser b
pright pa pb = pmap snd (pseq pa pb)

pleft :: Parser a -> Parser b -> Parser a
pleft pa pb = pmap fst (pseq pa pb)

paddRParen :: Parser a -> Parser a
paddRParen p = pleft p (pSym RParen)

pParened :: Parser a -> Parser a
pParened p = pleft (pright (pSym LParen) p) (pSym RParen)

pstar :: Parser a -> Parser [a]
pstar p ts = case p ts of
  Nothing -> Just ([], ts)
  Just (r1, ts1) -> case pstar p ts1 of
    Just (rs, rest) -> Just (r1 : rs, rest)
    Nothing -> error "pstar returns Nothing" -- Just ([r1], ts1)

pplus :: Parser a -> Parser [a]
pplus p ts = case pseq p (pstar p) ts of
  Just ((r1, rs), rest) -> Just (r1 : rs, rest)
  Nothing -> Nothing

pSym :: Token -> Parser Token
pSym t (h : rest) 
  | t == h = Just (h, rest)
  | otherwise = Nothing
pSym _ [] = Nothing

pName :: Parser String
pName ts = case ts of
  NameTok n : ts -> Just (n, ts)
  _  -> Nothing 

pProgram :: Parser Program
pProgram = pplus pStmt

pStmt :: Parser Statement 
pStmt ts = case ts of
  LParen : DataKW : rest -> pmap DataDSTMT (paddRParen pData) rest
  LParen : DefKW : rest -> pmap FnDSTMT (paddRParen pFn) rest
  LParen : Colon : rest -> pmap DeclSTMT (paddRParen pTypeDecl) rest
  _ -> Nothing

pData :: Parser DataDef 
pData = pseq pName (pplus pConstructor)

pConstructor :: Parser Constructor
pConstructor ts = case ts of
  NameTok n : rest -> Just ((n, []), rest)
  _ -> pParened (pseq pName (pplus pTypeSig)) ts

pTypeSig :: Parser TypeSig
pTypeSig ts = case ts of
  NameTok n : rest -> Just (AtomTS n, rest)
  LParen : Arrow : r1 -> 
    case paddRParen (pplus pTypeSig) r1 of
      Just (ts, rest) -> Just (multiArrowTS ts, rest)
      _ -> Nothing 
  _ -> Nothing

multiArrowTS :: [TypeSig] -> TypeSig
multiArrowTS [t] = t
multiArrowTS (t1 : ts) = ArrowTS t1 (multiArrowTS ts)

pPattern :: Parser Pattern
pPattern ts = case ts of
  NameTok n : rest -> Just ([n], rest)
  LParen : r1 -> paddRParen (pplus pName) r1

pFn :: Parser FnDef
pFn ts = case pseq pPattern pExpr ts of
  Just ((name : params, body), rest) -> Just ((name, params, body), rest)
  _ -> Nothing

pTypeDecl :: Parser TypeDecl 
pTypeDecl = pseq pName pTypeSig

pExpr :: Parser Expr
pExpr ts = case ts of
  IntTok n : rest -> Just (IntLitE n, rest)
  NameTok n : rest -> Just (VarE n, rest)
  LParen : NameTok n : r1 -> 
    case paddRParen (pstar pExpr) r1 of
      Just (es, rest) -> Just (foldl ApE (VarE n) es, rest)
      Nothing -> error "pstar in pExpr returns Nothing"
  LParen : CaseKW : r1 -> 
    case paddRParen (pseq pExpr (pplus pBranch)) r1 of
      Just ((e, bs), rest) -> Just (CaseE e bs, rest)
      Nothing -> Nothing
  _ -> Nothing

pBranch :: Parser Branch
pBranch ts = case ts of
  LParen : r1 -> paddRParen (pseq pPattern pExpr) r1
  _ -> Nothing 

parse :: String -> Program
parse s = case pProgram (tlex s) of
  Just (p, []) -> p
  _ -> error "ParseError"
