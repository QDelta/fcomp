module Main where

import System.Environment (getArgs)
import Parser
import Type.Check

main :: IO ()
main = do
  srcFile : _ <- getArgs 
  prog <- readFile srcFile
  print $ (typeCheckProgram . parse) prog
