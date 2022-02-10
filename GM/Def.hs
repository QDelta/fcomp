module GM.Def where

import Utils.Map
import Common.Def

data Instruction
  = PushG  Name  -- global
  | PushL  Ident -- lifted
  | PushI  Int   -- boxed int
  | Push   Int
  | Pop    Int
  | MkApp
  | Update Int
  | Pack   Int Int
  | Split
  | CaseJ  [(Int, [Instruction])]
  | Slide  Int
  | Alloc  Int
  | Eval
  | Add  | Sub  | Mul  | Div  | Rem
  | IsEq | IsGt | IsLt | IsNe | IsGe | IsLe
  | Not
  deriving (Show)

data Node
  = NApp Addr Addr
  | NGlobal Int Code
  | NInd Addr
  | NData Int [Addr]
  | NInt Int
  deriving (Show)

type Addr = Int
type Code = [Instruction]
type GlobalMap = Map String Addr
