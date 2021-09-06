module Main where

import System.Environment (getArgs)
import Parser.Parser
import Type.CoreGen
import GM.Compile
import CodeGen.CGen

template :: FilePath
template = "template.c"

main :: IO ()
main = do
  srcFile : dstFile : _ <- getArgs 
  prog <- readFile srcFile
  template <- readFile template
  let code = (codeGen . compile . genCore . parse) prog
  writeFile dstFile (template ++ code)
