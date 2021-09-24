module Main where

import System.Environment (getArgs)
import Parser.Parser
import Type.Inf
import Core.Gen
import GM.Compile
import GM.Optimize
import qualified CodeGen.CGen as MS
import qualified CodeGen.CGenCP as CP

runtimeMS, runtimeCP :: FilePath 
runtimeMS = "vm.c"
runtimeCP = "vm-cp.c"

-- defaultDstFile :: FilePath
-- defaultDstFile = "build/main.c"
dstMS, dstCP :: FilePath 
dstMS = "build/main-ms.c"
dstCP = "build/main-cp.c"

main :: IO ()
main = do
  srcFile : restArgs <- getArgs
  -- let dstFile = case restArgs of { [] -> defaultDstFile; h : _ -> h }
  progText <- readFile srcFile

  rtCodeMS <- readFile runtimeMS
  rtCodeCP <- readFile runtimeCP

  let prog = parse progText
  let core = (optimize . compile . genCore) prog
  putStrLn $ infer prog

  -- writeFile dstFile (rtCode ++ code)
  writeFile dstMS (rtCodeMS ++ MS.codeGen core)
  writeFile dstCP (rtCodeCP ++ CP.codeGen core)
