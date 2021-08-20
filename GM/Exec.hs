module GM.Exec where

import Utils
import GM.Inst
import GM.Heap

data Node
  = NApp Addr Addr
  | NGlobal String
  | NInd Addr

type GlobalMap = Map String Addr
type Stack = [Addr]

type State = ([GMInst], Stack, Heap Node, GlobalMap)

step :: State -> State

step ((PushG name) : i, s, h, m)
  = (i, mLookUp m name : s, h, m)

step ((Push offset) : i, s, h, m)
  = (i, (s !! offset) : s, h, m)

step (MkApp : i, a0 : a1 : s, h, m)
  = (i, a : s, newH, m)
  where (newH, a) = hAlloc h (NApp a0 a1)
