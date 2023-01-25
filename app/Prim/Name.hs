module Prim.Name where

import qualified Data.Map as M

import Common.Def

primNames :: [(RdrName, Name)]
primNames =
  [ ("add",   Name ("add",   0 ))
  , ("sub",   Name ("sub",   1 ))
  , ("mul",   Name ("mul",   2 ))
  , ("div",   Name ("div",   3 ))
  , ("rem",   Name ("rem",   4 ))
  , ("eq",    Name ("eq",    5 ))
  , ("gt",    Name ("gt",    6 ))
  , ("lt",    Name ("lt",    7 ))
  , ("ne",    Name ("ne",    8 ))
  , ("ge",    Name ("ge",    9 ))
  , ("le",    Name ("le",    10))
  , ("or",    Name ("or",    11))
  , ("and",   Name ("and",   12))
  , ("not",   Name ("not",   13))
  , ("False", Name ("False", 14))
  , ("True",  Name ("True",  15))
  , ("Nil",   Name ("Nil",   16))
  , ("Cons",  Name ("Cons",  17))
  ]

primNameMap :: M.Map RdrName Name
primNameMap = M.fromList primNames

primBranchNum :: M.Map Ident Int
primBranchNum = M.fromList
  [ (14, 2)
  , (15, 2)
  , (16, 2)
  , (17, 2)
  ]

minPrimIdent :: Ident
minPrimIdent = 0

maxPrimIdent :: Ident
maxPrimIdent = 17