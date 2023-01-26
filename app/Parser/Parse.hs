module Parser.Parse (parse) where

import Utils.Parsec
import Common.Def
import Common.AST
import Parser.Lexer
import Parser.Parser

parse :: String -> Program RdrName
parse s =
  case runParser pProgram (tlex s) of
    Just (p, []) -> p
    _ -> error "ParseError"