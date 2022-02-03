{-# LANGUAGE LambdaCase #-}

module Parser.Parser where

import Utils.Parsec
import Common.Def
import Common.AST

data Token
  = LParen | RParen
  | LBrace | RBrace
  | DataKW | CaseKW | OfKW  | LetKW | InKW
  | Arrow  | SemiC  | Equal | Or    | BSlash
  | LineComment
  | UNameTok String
  | LNameTok String
  | IntTok Int
  deriving (Show, Eq)

type TParser a = Parser Token a

pUName :: TParser RdrName
pUName = Parser $ \case
  UNameTok n : ts -> Just (n, ts)
  _ -> Nothing

pLName :: TParser RdrName
pLName = Parser $ \case
  LNameTok n : ts -> Just (n, ts)
  _ -> Nothing

pName :: TParser RdrName
pName = pUName <|> pLName

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

type Definition = Either (DataDef RdrName) (FnDef RdrName)
type PreProgram = [Definition]

pProgram :: TParser PreProgram
pProgram = pinterleave pDef pSemiC

pDef :: TParser Definition
pDef = (Left <$> pData) <|> (Right <$> pFn)

pData :: TParser (DataDef RdrName)
pData = do
  psym DataKW
  n <- pUName
  params <- pstar pLName
  psym Equal
  cs <- pinterleave pConstructor (psym Or)
  return (DataDef n params cs)

pConstructor :: TParser (Constructor RdrName)
pConstructor = do
  n <- pUName
  ts <- pstar pTypeSig
  return (n, ts)

pTypeSig :: TParser TypeSig
pTypeSig = do
  ts <- pinterleave pTSTerm (psym Arrow)
  return (foldr1 ArrTS ts)

pTSTerm :: TParser TypeSig
pTSTerm =
  (do
    dName <- pUName
    ts <- pstar pTSAtom
    return (DataTS dName ts)
  ) <|> pTSAtom

pTSAtom :: TParser TypeSig
pTSAtom = (VarTS <$> pLName) <|> pparened pTypeSig

pFn :: TParser (FnDef RdrName)
pFn = do
  name <- pLName
  params <- pstar pLName
  psym Equal
  FnDef name params <$> pExpr

pExpr :: TParser (Expr RdrName)
pExpr =
  (do
    psym CaseKW
    e <- pExpr
    psym OfKW
    brs <- pbraced $ pinterleave pBranch pSemiC
    return (CaseE e brs)
  ) <|>
  (do
    psym LetKW
    binds <- pbraced $ pinterleave pBind pSemiC
    psym InKW
    LetE binds <$> pExpr
  ) <|>
  -- (do
  --   psym BSlash
  --   params <- pplus pLName
  --   psym Arrow
  --   LambdaE params <$> pExpr
  -- ) <|>
  (do
    es <- pplus pExprAtom
    return (foldl1 AppE es)
  )

pExprAtom :: TParser (Expr RdrName)
pExprAtom =
  IntE <$> pInt <|>
  VarE <$> pName <|>
  pparened pExpr

pBind :: TParser (Bind RdrName)
pBind = do
  n <- pLName
  psym Equal
  e <- pExpr
  return (n, e)

pBranch :: TParser (Branch RdrName)
pBranch = do
  constr <- pUName
  binds <- pstar pLName
  psym Arrow
  expr <- pExpr
  return (constr, binds, expr)
