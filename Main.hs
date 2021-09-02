module Main where

import System.Environment (getArgs)
import Utils
import Parser.Parser
import Type.Check
import GM.Compile
import GM.Exec
import CodeGen.CGen

runProg :: Int -> String -> [String]
runProg n = runWithLog . initialState n . compile . typeCheckProgram . parse

template :: FilePath
template = "template.c"

main :: IO ()
main = do
  srcFile : dstFile : _ <- getArgs 
  prog <- readFile srcFile
  template <- readFile template
  let cp = (compile . typeCheckProgram . parse) prog
  writeFile dstFile (template ++ codeGen cp)
