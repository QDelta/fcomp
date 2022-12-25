module Prim.Type where

import Utils.Set
import Utils.Map
import Type.Def

mIntT, mBoolT :: MType
mIntT = DataT "Int" []
mBoolT = DataT "Bool" []

primFnTypes :: Map String PType
primFnTypes = mFromList
  [ ("add", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("sub", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("mul", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("div", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("rem", Forall emptySet $ ArrT mIntT (ArrT mIntT mIntT)),
    ("eq",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("gt",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("lt",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("ne",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("ge",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("le",  Forall emptySet $ ArrT mIntT (ArrT mIntT mBoolT)),
    ("or",  Forall emptySet $ ArrT mBoolT (ArrT mBoolT mBoolT)),
    ("and", Forall emptySet $ ArrT mBoolT (ArrT mBoolT mBoolT)),
    ("not", Forall emptySet $ ArrT mBoolT mBoolT)
  ]

primDataAttrs :: Map String DataAttr
primDataAttrs = mFromList
  [ ("Int",  (0, True)),
    ("Bool", (0, True)),
    ("List", (1, False))
  ]

primConstrTypes :: Map String PType
primConstrTypes = mFromList
  [ ("False", Forall emptySet mBoolT),
    ("True",  Forall emptySet mBoolT),
    ("Nil",   Forall (sSingleton 0) $ DataT "List" [VarT 0]),
    ("Cons",  Forall (sSingleton 0) $ ArrT (VarT 0) $ ArrT (DataT "List" [VarT 0]) (DataT "List" [VarT 0]))
  ]