module Main where

import System.Environment (getArgs)
import Parser.Parser
import Type.Inf
import Type.CoreGen
import GM.Compile
import CodeGen.CGen

template :: FilePath
template = "template.c"

dstFile :: FilePath
dstFile = "build/main.c"

main :: IO ()
main = do
  srcFile : _ <- getArgs 
  progText <- readFile srcFile
  template <- readFile template
  let prog = parse progText
  let code = (codeGen . compile . genCore) prog
  putStrLn $ infer prog
  writeFile dstFile (template ++ code)
