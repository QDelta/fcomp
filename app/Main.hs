module Main where

import System.Environment (getArgs)
import Parser.Parse
import Parser.Renamer
import Type.Inf
import Core.Gen
import Core.Optimize
import GM.Compile
import GM.Optimize
import CodeGen.CGen

runtime :: FilePath
runtime = "vm.c"

main :: IO ()
main = do
  srcFile : dstFile : restArgs <- getArgs

  progText <- readFile srcFile
  rtCode <- readFile runtime

  let prog = (rename . parse) progText
  putStrLn $ infer prog
  let progCode = (codeGen . optGM . compile . optCore . genCore) prog

  writeFile dstFile (rtCode ++ progCode)