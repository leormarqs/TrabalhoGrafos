module Graph.SpanningTree where

import Data.List
import Graph.Graph


spanningTree :: Graph -> Graph
spanningTree g = spanning inRegion outRegion es (Graph n [])
  where
    inRegion  = [n']
    outRegion = ns
    n@(n':ns) = getNodes g
    es@(e:_)  = getEdges g


spanning :: [NodeId] -> [NodeId] -> [Edge] -> Graph -> Graph
spanning    _        []       _   graph = graph
spanning inRegion outRegion edges graph = do
  let e = sort $ inOutEdges inRegion outRegion edges
      (s:t:_) = nodesOf (head e)

      (i,o)   = if elem s inRegion
                then (t:inRegion , delete t outRegion)
                else (s:inRegion , delete s outRegion)

  if null e
    then error "Disconnected Graph"
    else spanning i o edges (insertEdge (head e) graph)
         
  
inOutEdges :: [NodeId] -> [NodeId] -> [Edge] -> [Edge]
inOutEdges    []        _       _   = []
inOutEdges inRegion outRegion edges = do
  let
    (n:ns)     = inRegion
    es         = incidentEdges n (Graph inRegion edges)
    es'        = filter (\x -> isInOut x inRegion outRegion) es
      
    isInOut :: Edge -> [NodeId] -> [NodeId] -> Bool
    isInOut (Edge _ (s:t:_) _) i o =
      ((elem s i) && (elem t o)) ||
      ((elem s o) && (elem t i))
  
  if null es'
    then inOutEdges ns outRegion edges
    else es' ++ inOutEdges ns outRegion edges
