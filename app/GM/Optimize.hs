module GM.Optimize (optGM) where

import GM.Def
import GM.Compile
import Utils.Function

optGM :: CompiledCore -> CompiledCore
optGM (constrs, fns, lfns) =
  (map optConstr constrs, map optFn fns, map optLiftedFn lfns)

optConstr :: CompiledCoreConstr -> CompiledCoreConstr
optConstr (name, arity, tag, code) = (name, arity, tag, optIns code)

optFn :: CompiledCoreFn -> CompiledCoreFn
optFn (name, arity, code) = (name, arity, optIns code)

optLiftedFn :: CompiledLiftedFn -> CompiledLiftedFn
optLiftedFn (id, arity, code) = (id, arity, optIns code)

optIns :: [Instruction] -> [Instruction]
optIns = peephole . peephole

peephole :: [Instruction] -> [Instruction]
peephole code =
  case code of
    [] -> []
    Slide n : rest | n == 0 ->
      peephole rest
    Pop n   : rest | n == 0 ->
      peephole rest
    Pop n : Pop m : rest ->
      peephole $ Pop (n + m) : rest
    Slide n : Pop m : rest ->
      peephole $ Pop (n + m) : rest
    Slide n : Slide m : rest ->
      peephole $ Slide (n + m) : rest
    Eval : Slide n : rest ->
      peephole $ Slide n : Eval : rest
    Eval : Update n : Pop m : rest | n >= m ->
      peephole $ Slide m : Eval : Update (n - m) : rest
    CaseJ brs : rest ->
      [CaseJ $ map (second (peephole . (++ rest))) brs] -- merge rest instructions into branch
    ins : rest -> ins : peephole rest