module Prim.Core where

import Core.Def

primCoreConstrs :: [(String, Int, Int)]
primCoreConstrs =
  [ ("False", 0, 0),
    ("True",  0, 1),
    ("Nil",   0, 0),
    ("Cons",  2, 1)
  ]