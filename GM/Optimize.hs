module GM.Optimize (optGM) where

import GM.Def
import GM.Compile
import Utils.Function

optGM :: CompiledCore -> CompiledCore
optGM (constrs, fns) = (map optConstr constrs, map optFn fns)

optConstr :: CompiledCoreConstr -> CompiledCoreConstr
optConstr (name, arity, tag, code) = (name, arity, tag, peephole code)

optFn :: CompiledCoreFn -> CompiledCoreFn
optFn (name, arity, code) = (name, arity, peephole code)

peephole :: [Instruction] -> [Instruction]
peephole code =
  case code of
    [] -> []
    Slide n : rest | n == 0 ->
      peephole rest
    Pop n   : rest | n == 0 ->
      peephole rest
    Eval : Slide n : rest ->
      peephole (Slide n : Eval : rest)
    Eval : Update n : Pop m : rest | n >= m ->
      peephole (Slide m : Eval : Update (n - m) : rest)
    Jump brs : rest ->
      [Jump $ map (second (peephole . (++ rest))) brs] -- merge rest instructions into branch
    ins : rest -> ins : peephole rest
