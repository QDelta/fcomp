module Main where

import System.Environment (getArgs)
import Utils
import Parser.Parser
import Type.Check
import GM.Compile
import GM.Exec

runProg :: String -> [String]
runProg = runWithLog . initialState . typeCheckProgram . parse

main :: IO ()
main = do
  srcFile : dstFile : _ <- getArgs 
  prog <- readFile srcFile
  let states = runProg prog
  print (length states)
  writeFile dstFile (concat $ interleave "\n" states)
  