{-# LANGUAGE LambdaCase #-}

module Parser.Parser where

import Utils.Parsec
import Common.Def
import Common.AST

data Token
  = LParen | RParen
  | DataKW | MatchKW | WithKW | EndKW
  | ValKW | RecKW
  | LetKW | InKW
  | Arrow | Equal | Or | BSlash
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
pparened p =
  do psym LParen
     a <- p
     psym RParen
     return a

pProgram :: TParser (Program RdrName)
pProgram =
  do datas <- pstar pDataGroup
     groups <- pplus pGroup
     return (datas, groups)

pDataGroup :: TParser (DataGroup RdrName)
pDataGroup =
  do psym DataKW
     binds <- pinterleave pData (psym WithKW)
     return (RecData binds)

pData :: TParser (DataBind RdrName)
pData =
  do n <- pUName
     params <- pstar pLName
     psym Equal
     cs <- pinterleave pConstructor (psym Or)
     return (n, params, cs)

pGroup :: TParser (ValGroup RdrName)
pGroup =
  (do psym ValKW
      ValDef <$> pBind)
  <|>
  (do psym RecKW
      psym ValKW
      vals <- pinterleave pBind (psym WithKW)
      return (RecVal vals))

pBind :: TParser (Bind RdrName)
pBind =
  do n <- pLName
     ps <- pstar pLName
     psym Equal
     b <- pExpr
     return (n, if null ps then b else LambdaE ps b)

pConstructor :: TParser (Constructor RdrName)
pConstructor =
  do n <- pUName
     ts <- pstar pTypeSig
     return (n, ts)

pTypeSig :: TParser TypeSig
pTypeSig =
  do ts <- pinterleave pTSTerm (psym Arrow)
     return (foldr1 ArrTS ts)

pTSTerm :: TParser TypeSig
pTSTerm =
  (do dName <- pUName
      ts <- pstar pTSAtom
      return (DataTS dName ts)) <|>
  pTSAtom

pTSAtom :: TParser TypeSig
pTSAtom = (VarTS <$> pLName) <|> pparened pTypeSig

pExpr :: TParser (Expr RdrName)
pExpr =
  (do psym MatchKW
      e <- pExpr
      psym WithKW
      brs <- pplus pBranch
      pmaybe (psym EndKW)
      return (CaseE e brs)) <|>
  (do psym LetKW
      bind <- pBind
      psym InKW
      LetE bind <$> pExpr) <|>
  (do psym LetKW
      psym RecKW
      binds <- pinterleave pBind (psym WithKW)
      psym InKW
      LetRecE binds <$> pExpr) <|>
  (do psym BSlash
      params <- pplus pLName
      psym Arrow
      LambdaE params <$> pExpr) <|>
  (do es <- pplus pExprAtom
      return (foldl1 AppE es))

pExprAtom :: TParser (Expr RdrName)
pExprAtom =
  IntE <$> pInt <|>
  VarE <$> pName <|>
  pparened pExpr

pBranch :: TParser (Branch RdrName)
pBranch =
  do psym Or
     constr <- pUName
     binds <- pstar pLName
     psym Arrow
     expr <- pExpr
     return (constr, binds, expr)