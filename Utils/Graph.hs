module Utils.Graph
  ( Graph,
    emptyGraph,
    -- gSuccs,
    -- gAddVertex,
    gAddEdge,
    gElem,
    gVertices,
    gEdges,
    gIsConn,
    gIsWeakConn,
    gReflClosure,
    gTransClosure,
    -- gSymmClosure,
    -- gTranspose,
    -- gWCCs,
    -- gDFS,
    gDFS1,
    gSubGraph
  ) where

import Utils.Function
import Utils.Set
import Utils.Map

newtype Graph v = Graph (Map v (Set v)) deriving Show

emptyGraph :: Graph v
emptyGraph = Graph emptyMap

gSuccs :: Ord v => Graph v -> v -> Set v
gSuccs (Graph adj) ver = mLookup adj ver emptySet

gAddVertex :: Ord v => Graph v -> v -> Graph v
gAddVertex (Graph adj) v = Graph $
  if v `mElem` adj then adj else mInsert adj (v, emptySet)

gAddEdge :: Ord v => Graph v -> (v, v) -> Graph v
gAddEdge (Graph adj) (src, dst) = Graph newAdj
  where 
    adj1 = 
      if src `mElem` adj then
        mUpdate (`sInsert` dst) src adj
      else
        mInsert adj (src, sSingleton dst)
    newAdj = 
      if dst `mElem` adj1 then
        adj1
      else
        mInsert adj1 (dst, emptySet)

gElem :: Ord v => v -> Graph v -> Bool
gElem v (Graph adj) = v `mElem` adj

gVertices :: Ord v => Graph v -> [v]
gVertices (Graph adj) = map fst $ mToList adj

gEdges :: Ord v => Graph v -> [(v, v)]
gEdges (Graph adj) = flatSet $ mToList adj
  where
    flatSet [] = []
    flatSet ((v, adjs) : rest) = zip (repeat v) (sToList adjs) ++ flatSet rest

gIsConn :: Ord v => Graph v -> (v, v) -> Bool
gIsConn (Graph adj) (src, dst) = 
  case mLookupMaybe adj src of
    Just s -> dst `sElem` s
    Nothing -> False

gIsWeakConn :: Ord v => Graph v -> (v, v) -> Bool
gIsWeakConn g (src, dst) = gIsConn g (src, dst) || gIsConn g (dst, src)

gReflClosure :: Ord v => Graph v -> Graph v
gReflClosure g = foldl gAddEdge g reflPairs
  where
    vList = gVertices g
    reflPairs = zip vList vList

gTransClosure :: Ord v => Graph v -> Graph v
gTransClosure g = warshall vList g
  where
    vList = gVertices g
    vPairs = cartProdL vList vList
    warshall [] g = g
    warshall (v : vs) g = 
      warshall vs $ foldl gAddEdge g (filter test vPairs)
      where
        test (v1, v2) = gIsConn g (v1, v) && gIsConn g (v, v2)
    cartProdL :: [a] -> [a] -> [(a, a)]
    cartProdL [] _ = []
    cartProdL _ [] = []
    cartProdL (x : xs) ys = zip (repeat x) ys ++ cartProdL xs ys

gTranspose :: Ord v => Graph v -> Graph v
gTranspose g = 
  foldl gAddEdge emptyGraph $ map (\(a, b) -> (b, a)) (gEdges g)

gSymmClosure :: Ord v => Graph v -> Graph v
gSymmClosure g = 
  foldl gAddEdge g $ map (\(a, b) -> (b, a)) (gEdges g)

-- gWCCs :: Ord v => Graph v -> [[v]]
-- gWCCs = gDFS . gSymmClosure

-- gDFS :: Ord v => Graph v -> [[v]]
-- gDFS g = fst $ _dfs g ([], sFromList $ gVertices g)

-- _dfs :: Ord v => Graph v -> ([[v]], Set v) -> ([[v]], Set v)
-- _dfs g (vls, unvis) =
--   if sIsEmpty unvis then 
--     (vls, unvis)
--   else
--     let 
--       (ver, unvis1) = sRemoveMin unvis
--       (vs, newUnvis) = _dfs1 g unvis1 ver
--     in
--       _dfs g (vs : vls, newUnvis)

-- (visited, unvisited)
gDFS1 :: Ord v => Graph v -> v -> ([v], Set v)
gDFS1 g = _dfs1 g (sFromList $ gVertices g)

_dfs1 :: Ord v => Graph v -> Set v -> v -> ([v], Set v)
_dfs1 g unvis ver = if ver `sElem` unvis then (vs, newUnvis) else ([], unvis)
  where
    unvisited = unvis `sRemove` ver
    succs = sToList $ gSuccs g ver
    (vls, newUnvis) = mapAccumL (_dfs1 g) unvisited succs
    vs = ver : concat vls

gSubGraph :: Ord v => Graph v -> Set v -> Graph v
gSubGraph (Graph adj) vset = Graph newAdj
  where
    adjList = mToList adj
    adjl1 = filter ((`sElem` vset) . fst) adjList
    newAdj = mFromList (map (second (`sIntersect` vset)) adjl1)

