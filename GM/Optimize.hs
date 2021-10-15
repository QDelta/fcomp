module GM.Optimize (optimize) where

import GM.Def
import GM.Compile

optimize :: CompiledCore -> CompiledCore
optimize = id
-- optimize (constrs, fns) = (constrs, map optFn fns)

-- optFn :: CompiledCoreFn -> CompiledCoreFn
-- optFn (name, arity, code) = (name, arity, (peephole . rmRedundant) code)

-- -- Slide 0; Pop 0
-- rmRedundant :: [Instruction] -> [Instruction]
-- rmRedundant code = reverse (rmr code [])
--   where
--     rmr [] opted = opted
--     rmr (i1 : r1) opted =
--       case i1 of
--         Slide n | n == 0 -> rmr r1 opted
--         Pop   n | n == 0 -> rmr r1 opted
--         Jump brs -> rmr r1 (Jump (map rmrBrs brs) : opted)
--         i1 -> rmr r1 (i1 : opted)
--     rmrBrs (tag, br) = (tag, rmRedundant br)

-- -- Eval, Slide -> Slide Eval; Eval, Update, Pop -> Slide, Eval, Update
-- peephole :: [Instruction] -> [Instruction]
-- peephole code = reverse (ph code [])
--   where
--     ph [] opted = opted
--     ph (i1 : r1) opted =
--       case i1 of
--         Eval -> case r1 of
--           Slide n : r2 -> ph r2 (Slide n : Eval : opted)
--           Update n : Pop m : r2 | n >= m ->
--             ph r2 (Update (n - m) : Eval : Slide m : opted)
--           r1 -> ph r1 (i1 : opted)
--         Jump brs -> ph r1 (Jump (map phBrs brs) : opted)
--         i1 -> ph r1 (i1 : opted)
--     phBrs (tag, br) = (tag, peephole br)
