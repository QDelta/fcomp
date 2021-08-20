module GM.Heap where

import Utils

type Addr = Int
type Heap a = (Map Addr a, [Addr])

initialHeap :: Heap a
initialHeap = (emptyMap, [1..])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (m, a : r) n = ((mInsert m (a, n), r), a)

hLookUp :: Heap a -> Addr -> a
hLookUp (m, _) = mLookUp m

