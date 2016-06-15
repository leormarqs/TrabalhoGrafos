module Graph.SpanningTree (
  spanningTree
  ) where

import Data.List
import Graph.Graph

--Initialize the Minimum Spanning Tree Calc
--Return Nothing if g is disconnected
spanningTree :: Graph -> Maybe Graph
spanningTree g = spanning close open es (newGraph n [])
  where
    close       = [n']
    n@(n':open) = getNodes g
    es          = getEdges g

--Implement the Prim's Algorithm
--Return Nothing if graph is disconnected
spanning :: [NodeId] -> [NodeId] -> [Edge] -> Graph -> Maybe Graph
spanning    _        []       _   graph = Just graph
spanning close open edges graph = do
  let
    --get all open/close edges
    e = sort $ openCloseEdges close open edges
    --get nodes of the lower open/close edge
    (s:t:_)   = nodesOf (head e)
    --close nodes properly
    (cls,opn) = if s `elem` close
                then (t:close , delete t open)
                else (s:close , delete s open)

  if null e
    then Nothing --Disconnected Graph
    else spanning cls opn edges (insertEdge (head e) graph)

--get all edges that connects a closed node with a open node
openCloseEdges :: [NodeId] -> [NodeId] -> [Edge] -> [Edge]
openCloseEdges    []          _       _   = []
openCloseEdges close@(n:ns)  open   edges = do
  let es     = incidentEdges n (buildGraph close edges)
      es'    = filter (\x -> isOpenClose (nodesOf x) close open) es

      --verify if a edge connects a closed node with a open node
      isOpenClose :: [NodeId] -> [NodeId] -> [NodeId] -> Bool
      isOpenClose (s:t:_) close open =
        (elem s close && elem t open) ||
        (elem s open && elem t close)

  es' ++ openCloseEdges ns open edges
