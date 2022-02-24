module Parser.Parse (parse) where

import Utils.Parsec
import Common.Def
import Common.AST
import Parser.Lexer
import Parser.Parser
import Utils.Function

parse :: String -> Program RdrName
parse s = case runParser pProgram (tlex s) of
  Just (p, []) -> reorder p
  _ -> error "ParseError"
  where
    reorder [] = ([], [])
    reorder (Left  dDef : rest) = first  (dDef :) $ reorder rest
    reorder (Right fDef : rest) = second (fDef :) $ reorder rest