module GM where

import Utils

data GMInst
  = PushI  Int
  | PushG  String
  | Push   Int
  | Pop    Int
  | MkApp 
  | Update Int
  | Pack   Int Int
  | Split
  | Jump   (Map Int [GMInst])
  | Slide  Int
  | Eval
  | Alloc  Int
  | Unwind

