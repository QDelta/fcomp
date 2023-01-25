module Prim.Type where

import qualified Data.Map as M

import Type.Def

mIntT, mBoolT :: MType
mIntT = DataT "Int" []
mBoolT = DataT "Bool" []

primFnTypes :: M.Map String PType
primFnTypes = M.fromList
  [ ("add", Forall [] $ ArrT mIntT (ArrT mIntT mIntT)),
    ("sub", Forall [] $ ArrT mIntT (ArrT mIntT mIntT)),
    ("mul", Forall [] $ ArrT mIntT (ArrT mIntT mIntT)),
    ("div", Forall [] $ ArrT mIntT (ArrT mIntT mIntT)),
    ("rem", Forall [] $ ArrT mIntT (ArrT mIntT mIntT)),
    ("eq",  Forall [] $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("gt",  Forall [] $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("lt",  Forall [] $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("ne",  Forall [] $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("ge",  Forall [] $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("le",  Forall [] $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("or",  Forall [] $ ArrT mBoolT (ArrT mBoolT mBoolT)),
    ("and", Forall [] $ ArrT mBoolT (ArrT mBoolT mBoolT)),
    ("not", Forall [] $ ArrT mBoolT mBoolT)
  ]

primDatas :: M.Map String Int
primDatas = M.fromList
  [ ("Int",  0),
    ("Bool", 0),
    ("List", 1)
  ]

primConstrTypes :: M.Map String PType
primConstrTypes = M.fromList
  [ ("False", Forall [] mBoolT),
    ("True",  Forall [] mBoolT),
    ("Nil",   Forall [0] $ DataT "List" [VarT 0]),
    ("Cons",  Forall [0] $ ArrT (VarT 0) $ ArrT (DataT "List" [VarT 0]) (DataT "List" [VarT 0]))
  ]