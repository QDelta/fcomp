module GM.Def where

import qualified Data.Map as M

import Common.Def

data Instruction
  = PushG  Name  -- global
  | PushL  Ident -- lifted
  | PushI  Int   -- integer
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

pack :: Int -> Int -> Instruction
pack tag arity = if arity == 0 then PushI tag else Pack tag arity

type Addr = Int
type Code = [Instruction]
type GlobalMap = M.Map String Addr