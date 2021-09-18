module Main where

import System.Environment (getArgs)
import Parser.Parser
import Type.Inf
import Core.Gen
import GM.Compile
import GM.Optimize
import CodeGen.CGen

runtimeC :: FilePath
runtimeC = "vm.c"

defaultDstFile :: FilePath
defaultDstFile = "build/main.c"

main :: IO ()
main = do
  srcFile : restArgs <- getArgs
  let dstFile = case restArgs of { [] -> defaultDstFile; h : _ -> h }
  progText <- readFile srcFile
  rtCode <- readFile runtimeC
  let prog = parse progText
  let code = (codeGen . optimize . compile . genCore) prog
  putStrLn $ infer prog
  writeFile dstFile (rtCode ++ code)
