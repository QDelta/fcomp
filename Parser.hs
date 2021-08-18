module Parser where

import Data.Char (isDigit, isSpace)
import AST

data Token 
  = LParen | RParen
  | LBrack | RBrack
  | DefKW  | DataKW | CaseKW
  | Colon
  | NumTok Int 
  | NameTok String
  deriving (Show, Eq)

tlex :: String -> [Token]
tlex s = case s of
  [] -> []
  c : cs
    | isSpace c -> tlex cs
    | c == ':'  -> Colon  : tlex cs
    | c == '('  -> LParen : tlex cs
    | c == ')'  -> RParen : tlex cs
    | c == '['  -> LBrack : tlex cs
    | c == ']'  -> RBrack : tlex cs
    | isDigit c -> let (i, rest) = numLex c cs in NumTok i : tlex rest
    | otherwise -> let (n, rest) = nameLex c cs in n : tlex rest

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c || c `elem` ":()[]")

numLex :: Char -> String -> (Int, String)
numLex c cs = (i, rest)
  where i = read (c : numr)
        (numr, rest) = span isDigit cs

nameLex :: Char -> String -> (Token, String)
nameLex c cs = (tn, rest)
  where n = c : name'
        (name', rest) = span isNameChar cs
        tn = case n of
          "def"  -> DefKW
          "data" -> DataKW
          "case" -> CaseKW
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

paddRBrack :: Parser a -> Parser a
paddRBrack p = pleft p (pSym RBrack)

pParened :: Parser a -> Parser a
pParened p = pleft (pright (pSym LParen) p) (pSym RParen)

pBracked :: Parser a -> Parser a
pBracked p = pleft (pright (pSym LBrack) p) (pSym RBrack)

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
pSym t (h : rest) | t == h = Just (h, rest)
pSym _ _ = Nothing

pName :: Parser String
pName ts = case ts of
  NameTok n : ts -> Just (n, ts)
  _  -> Nothing 

pProgram :: Parser Program
pProgram = pplus pDef

pDef :: Parser Definition
pDef ts = case ts of
  LParen : DataKW : rest -> paddRParen pData rest
  LParen : DefKW : rest -> paddRParen pFn rest
  _ -> Nothing

pData :: Parser Definition
pData ts = case pseq pName (pplus pConstructor) ts of
  Just ((n, ps), rest) -> Just (DataDef n ps, rest)
  Nothing -> Nothing

pConstructor :: Parser Constructor
pConstructor ts = case ts of
  NameTok n : rest -> Just ((n, []), rest)
  _ -> pParened (pseq pName (pplus pTypeSig)) ts

pTypeSig :: Parser TypeSig
pTypeSig ts = case ts of
  NameTok n : rest -> Just ([n], rest)
  _ -> pBracked (pplus pName) ts

pPattern :: Parser Pattern
pPattern ts = case ts of
  NameTok n : rest -> Just (VarP n, rest)
  _ -> case pParened (pseq pName (pstar pName)) ts of
    Just ((name, constrs), rest) -> Just (ConstrP name constrs, rest)
    Nothing -> Nothing

pParam :: Parser Parameter 
pParam = pBracked (pseq (pleft pName (pSym Colon)) (pplus pName))

pFn :: Parser Definition
pFn ts = case pseq pFnDecl pExpr ts of
  Just (((name, params), body), rest) -> Just (FnDef name params body, rest)
  Nothing -> Nothing

pFnDecl :: Parser (String, [Parameter])
pFnDecl ts = case ts of
  NameTok n : rest -> Just ((n, []), rest)
  LParen : NameTok n : r1 -> case paddRParen (pplus pParam) r1 of
    Just (ps, rest) -> Just ((n, ps), rest)
    Nothing -> Nothing

pExpr :: Parser Expr
pExpr ts = case ts of
  NumTok i : rest -> Just (ILitE i, rest)
  NameTok n : rest -> Just (VarE n, rest)
  LParen : NameTok n : r1 -> case paddRParen (pstar pExpr) r1 of
    Just (es, rest) -> Just (foldl ApE (VarE n) es, rest)
    Nothing -> error "pstar in pExpr returns Nothing"
  LParen : CaseKW : r1 -> case paddRParen (pseq pExpr (pplus pBranch)) r1 of
    Just ((e, bs), rest) -> Just (CaseE e bs, rest)
    Nothing -> Nothing
  _ -> Nothing

pBranch :: Parser Branch
pBranch ts = case ts of
  LParen : r1 -> paddRParen (pseq pPattern pExpr) r1
  _ -> Nothing 

parse :: String -> Program
parse s = case pProgram (tlex s) of
  Just (p, _) -> p
  Nothing -> error "ParseError"
