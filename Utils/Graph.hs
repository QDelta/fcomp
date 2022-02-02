module Utils.Graph (Graph, strongCCs) where

import Data.Graph (stronglyConnComp, flattenSCC)

type Graph k v = [(v, k, [k])] -- directed graph with adj list

-- strongly connected components in reverse topological order
strongCCs :: Ord k => Graph k v -> [[v]]
strongCCs g = map flattenSCC (stronglyConnComp g)
