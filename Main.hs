module Main where

import System.Environment (getArgs)
import Parser.Parse
import Parser.Renamer
import Type.Inf
import Core.Gen
import GM.Compile
import GM.Optimize
import CodeGen.CGen

runtime :: FilePath
runtime = "vm.c"

defaultDstFile :: FilePath
defaultDstFile = "build/main.c"

main :: IO ()
main = do
  srcFile : restArgs <- getArgs
  let dstFile = case restArgs of { [] -> defaultDstFile; h : _ -> h }

  progText <- readFile srcFile
  rtCode <- readFile runtime

  let prog = (rename . parse) progText
  let progCode = (codeGen . optGM . compile . genCore) prog
  putStrLn $ infer prog

  writeFile dstFile (rtCode ++ progCode)
