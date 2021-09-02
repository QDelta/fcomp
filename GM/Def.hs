module GM.Def where

import Utils
import GM.Heap

data Instruction
  = PushG  String
  | PushI  Int
  | Push   Int
  | Pop    Int
  | MkApp 
  | Update Int
  | Pack   Int Int
  | Split
  | Jump   (Map Int [Instruction])
  | Slide  Int
  | Eval
  | Alloc  Int
  | Unwind
  | Add  | Sub  | Mul  | Div  | Rem
  | IsEq | IsGt | IsLt
  | And  | Or   | Not
  deriving (Show)

data Node
  = NApp Addr Addr
  | NGlobal Int Code
  | NInd Addr
  | NData Int [Addr]
  | NInt Int
  deriving (Show)

type Code = [Instruction]
type GlobalMap = Map String Addr
type Stack = [Addr]
type Dump = [(Code, Stack)]

type State = (Code, Stack, Dump, Heap Node, GlobalMap)
