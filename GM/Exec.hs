module GM.Exec where

import Utils
import GM.Inst
import GM.Heap

data Node
  = NApp Addr Addr
  | NGlobal Int Code
  | NInd Addr
  | NData Int [Addr]
  deriving (Show)

type Code = [Instruction]
type GlobalMap = Map String Addr
type Stack = [Addr]
type Dump = [(Code, Stack)]

type State = (Code, Stack, Dump, Heap Node, GlobalMap)

step :: Instruction -> State -> State

step (PushG name) (i, s, d, h, m) =
  (i, mLookup m name : s, d, h, m)

step (Push offset) (i, s, d, h, m) =
  (i, (s !! offset) : s, d, h, m)

step MkApp (i, a0 : a1 : s, d, h, m) =
  (i, a : s, d, newH, m)
  where (newH, a) = hAlloc h (NApp a0 a1)

step Unwind (i, a : s, d, h, m) =
  case hLookUp h a of
    NApp a0 _   -> (Unwind : i, a0 : a : s, d,     h, m)
    NInd a'     -> (Unwind : i, a' : s,     d,     h, m)
    NGlobal n c 
      | length s >= n -> (c, args ++ drop n (a : s), d, h, m)
      | otherwise -> (prevI, last (a : s) : prevS, prevD, h, m)
      where 
        (prevI, prevS) : prevD = d
        args = map (getArg . hLookUp h) s
        getArg (NApp _ a1) = a1
    NData _ _   -> (prevI,      a  : prevS, prevD, h, m)
      where (prevI, prevS) : prevD = d

step (Update n) (i, a : s, d, h, m) =
  (i, s, d, hUpdate h (s !! n, NInd a), m)

step (Pack t n) (i, s, d, h, m) =
  (i, a : newS, d, newH, m)
  where
    (params, newS) = splitAt n s
    (newH, a) = hAlloc h (NData t params)

step Split (i, a : s, d, h, m) =
  case hLookUp h a of
    NData t params -> (i, params ++ s, d, h, m)
    _ -> error "GM: run split non-data"

step (Jump brs) (i, a : s, d, h, m) =
  case hLookUp h a of
    NData t _ -> (mLookup brs t ++ i, a : s, d, h, m)
    _ -> error "GM: run jump non-data"

step (Slide n) (i, a0 : s, d, h, m) =
  (i, a0 : drop n s, d, h, m)

step Eval (i, a : s, d, h, m) =
  ([Unwind], [a], (i, s) : d, h, m)

step (Alloc n) (i, s, d, h, m) =
  (i, newS, d, newH, m)
  where
    (newH, nullNodes) = mapAccumL hAlloc h (replicate n (NInd nullAddr))
    newS = nullNodes ++ s

step (Pop n) (i, s, d, h, m) =
  (i, drop n s, d, h, m)

run :: State -> State
run (i : is, s, d, h, m) = run $ step i (is, s, d, h, m)
run state@([], _, _, _, _) = state

showState :: State -> String
showState (i, s, d, h, m) =
  "Code: " ++ show i ++ "\n" ++
  "Stack: " ++ show s ++ "\n" ++
  "Dump: " ++ show d ++ "\n" ++
  "Heap: " ++ hShow h ++ "\n"
