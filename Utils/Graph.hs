module Utils.Graph (strongCCs) where

import Data.Graph (stronglyConnComp, flattenSCC)

-- strongly connected components in reverse topological order
strongCCs :: Ord k => [(v, k, [k])] -> [[v]]
strongCCs g = map flattenSCC (stronglyConnComp g)
