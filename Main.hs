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

templateC :: FilePath
templateC = "CodeGen/main_template.c"

dstDir :: FilePath 
dstDir = "CodeGen/"

main :: IO ()
main = do
  srcFile : _ <- getArgs 
  prog <- readFile srcFile
  template <- readFile templateC 
  let cp = (compile . typeCheckProgram . parse) prog
  writeFile (dstDir ++ "main.c") (template ++ codeGen cp)
  -- writeFile (dstDir ++ "run.out") (concat $ interleave "\n" ((runWithLog . initialState 0) cp))
