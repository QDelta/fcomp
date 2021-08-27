module GM.Heap where

import Utils

type Addr = Int
type Heap a = (Map Addr a, [Addr])

nullAddr :: Addr
nullAddr = -1

emptyHeap :: Heap a
emptyHeap = (emptyMap, [0..])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (m, a : r) n = ((mInsert m (a, n), r), a)

hLookup :: Heap a -> Addr -> a
hLookup (m, _) = mLookup m

hUpdate :: Heap a -> (Addr, a) -> Heap a
hUpdate (m, r) p = (mInsert m p, r)

hToList :: Heap a -> [(Addr, a)]
hToList = mToList . fst

hShow :: Show a => Heap a -> String
hShow = mShow . fst
