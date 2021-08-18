module Main where

import System.Environment (getArgs)
import Parser
import Type

main :: IO ()
main = do
  srcFile : _ <- getArgs 
  prog <- readFile srcFile
  print $ parse prog
