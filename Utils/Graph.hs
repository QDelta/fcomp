module Utils.Graph 
  ( Graph
  , SCC(..)
  , strongCCs
  , flattenSCC
  ) where

import qualified Data.Graph as G

type Graph k v = [(v, k, [k])] -- directed graph with adj list

data SCC v
  = CyclicSCC [v]
  | AcyclicSCC v

instance Functor SCC where
  fmap f (CyclicSCC  l) = CyclicSCC  (map f l)
  fmap f (AcyclicSCC e) = AcyclicSCC (f e)

flattenSCC :: SCC v -> [v]
flattenSCC (CyclicSCC  l) = l
flattenSCC (AcyclicSCC e) = [e]

-- strongly connected components in reverse topological order
strongCCs :: Ord k => Graph k v -> [SCC v]
strongCCs g = map convertSCC (G.stronglyConnComp g)
  where
    convertSCC (G.CyclicSCC  l) = CyclicSCC l
    convertSCC (G.AcyclicSCC e) = AcyclicSCC e