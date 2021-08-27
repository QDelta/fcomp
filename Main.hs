module Main where

import System.Environment (getArgs)
import Utils
import Parser.Parser
import Type.Check
import GM.Compile
import GM.Exec
import CodeGen.CGen

runProg :: String -> [String]
runProg = runWithLog . initialState . compile . typeCheckProgram . parse

main :: IO ()
main = do
  srcFile : dstFile : _ <- getArgs 
  prog <- readFile srcFile
  let cp = (compile . typeCheckProgram . parse) prog
  writeFile dstFile (codeGen cp)
  -- let states = runProg prog
  -- print (length states)
  -- writeFile dstFile (concat $ interleave "\n" states)
  