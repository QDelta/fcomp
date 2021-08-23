module GM.Heap where

import Utils

type Addr = Int
type Heap a = (Map Addr a, [Addr])

nullAddr :: Addr
nullAddr = 0

initialHeap :: Heap a
initialHeap = (emptyMap, [1..])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (m, a : r) n = ((mInsert m (a, n), r), a)

hLookUp :: Heap a -> Addr -> a
hLookUp (m, _) = mLookup m

hUpdate :: Heap a -> (Addr, a) -> Heap a
hUpdate (m, r) p = (mInsert m p, r)

hShow :: Show a => Heap a -> String
hShow (m, r) = show m
