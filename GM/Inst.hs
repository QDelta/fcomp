module GM.Inst where

import Utils

data Instruction
  = PushG  String
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
  deriving (Show)
