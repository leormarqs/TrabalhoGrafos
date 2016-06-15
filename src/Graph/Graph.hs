module Graph.Graph (
  Graph,
  NodeId,
  EdgeId,
  Edge,

  emptyGraph,
  newGraph,
  buildGraph,
  insertNode,
  insertEdge,
  newEdge,
  removeNode,
  removeEdge,
  removeParallel,

  getNodes,
  getEdges,

  labelOf,
  nodesOf,
  weightOf,

  incidentEdges,
  neighbourNodes,

  isAdjacentTo,
  isIncidentTo,
  isParallel,
  notParallel,
  isParGT,
  isParLT
  ) where

---------------------------------------------------------------------------------------

import Data.List

---------------------------------------------------------------------------------------

--Ids of nodes and edges abstraction
type NodeId = Int
type EdgeId = Int


--Data type for edges
data Edge = Edge {
                    label  :: EdgeId,
                    nodes' :: [NodeId],
                    weight :: Float
                  }


--Exibition of edges
instance Show Edge where
  show (Edge l [s, t] v) = " " ++ show s ++ "--" ++ show t ++ "(" ++ show v ++ ")\n"

--Order of edges
instance Ord Edge where
  compare (Edge _ n v) (Edge _ n' v') = compare (v,n) (v',n')

--Equality of Edges
instance Eq Edge where
  (==) (Edge _ n v) (Edge _ n' v') = n == n' && v == v'


--Data type for Graphs
data Graph = Graph {
                      nodes :: [NodeId],
                      edges :: [Edge]
                    } deriving (Eq)

--Exibition of Graphs
instance Show Graph where
  show (Graph n e) = "Nodes:\n" ++ show n ++ "\nEdges:\n" ++ show e

---------------------------------------------------------------------------------------

--Build a empty graph
emptyGraph :: Graph
emptyGraph = Graph [] []

--build a graph with pre-instantiated nodes and/or edges
buildGraph :: [NodeId] -> [Edge] -> Graph
buildGraph [] _ = error "Nodes list empty"
buildGraph n e = Graph (sort n) (sort e)

--build a graph with new nodes and/or edges
newGraph :: [NodeId] -> [(EdgeId, NodeId, NodeId,  Float)] -> Graph
newGraph [] _ = error "Nodes list empty."
newGraph n e  = Graph (sort n) (sort $ buildEdges e)
  where
    buildEdges :: [(EdgeId, NodeId, NodeId, Float)] -> [Edge]
    buildEdges [] = []
    buildEdges ((l,s,t,v):ts) = Edge l (sort [s,t]) v : buildEdges ts

--insert a node in a graph
insertNode :: NodeId -> Graph -> Graph
insertNode n graph =
  if n `elem` ns then graph else Graph (sort $ n:ns) es
  where
    ns = getNodes graph
    es = getEdges graph

--insert a existant edge in a graph
insertEdge :: Edge -> Graph -> Graph
insertEdge e graph =
  if e `elem` es then graph else Graph ns (sort $ e:es)
  where
    ns  = getNodes graph
    es  = getEdges graph

--insert a new edge in a graph
newEdge :: (EdgeId, NodeId, NodeId, Float) -> Graph -> Graph
newEdge (l,s,t,v) graph =
  if e `elem` es then graph else Graph ns (sort $ e:es)
  where
    ns  = getNodes graph
    es  = getEdges graph
    e   = Edge l (sort [s,t]) v

--remove a node and edges connected to it from a graph
removeNode :: NodeId -> Graph -> Graph
removeNode n (Graph ns es) = Graph ns' es'
  where
    ns' = filter (n /=) ns
    es' = filter (not . elem n . nodesOf) es

--remove a edge from a graph
removeEdge :: EdgeId -> Graph -> Graph
removeEdge e (Graph ns es) = Graph ns es'
  where
    es' = filter (\x -> e /= labelOf x) es

--remove parallel edges of a graph (keep the edge with lower value)
removeParallel :: Graph -> Graph
removeParallel (Graph n es) = Graph n (sort es')
  where
    es' = removePar es es
    removePar :: [Edge] -> [Edge] -> [Edge]
    removePar [] e' = e'
    removePar (e:t) e' = removePar t (filter (\x -> not $ isParLT e x) e')


---------------------------------------------------------------------------------------
--get Nodes from a graph
getNodes :: Graph -> [NodeId]
getNodes (Graph [] _) = error "Empty Nodes Graph"
getNodes (Graph n  _) = sort n

--get Edges from a graph
getEdges :: Graph -> [Edge]
getEdges (Graph _ e) = sort e

--get the label of a edge
labelOf :: Edge -> EdgeId
labelOf (Edge l _ _) = l

--get the nodes of a edge
nodesOf :: Edge -> [NodeId]
nodesOf (Edge _ n _) = sort n

--get the weight of a edge
weightOf :: Edge -> Float
weightOf (Edge _ _ w) = w

--find all incident edges on a node
incidentEdges :: NodeId -> Graph -> [Edge]
incidentEdges n (Graph _ es) = filter (elem n . nodesOf) es

--find all adjacents nodes from a node
neighbourNodes :: NodeId -> Graph -> [NodeId]
neighbourNodes n graph = filter (n/=) ns
  where
    es = incidentEdges n graph
    ns = concatMap nodesOf es

---------------------------------------------------------------------------------------

--verify if two nodes are adjacents
isAdjacentTo :: NodeId -> NodeId -> Graph -> Bool
isAdjacentTo n n' graph = elem n' $ neighbourNodes n graph

--verify if a edge is incident to a node
isIncidentTo :: Edge -> NodeId -> Bool
isIncidentTo e n = elem n $ nodesOf e

--Verify if two edges are parallels
isParallel :: Edge -> Edge -> Bool
isParallel (Edge _ n _) (Edge _ n' _)  = n == n'

--Verify if two edges aren't parallels
notParallel :: Edge -> Edge -> Bool
notParallel e e' = not $ isParallel e e'

--verify if two edges are parallel, if true, verify if first is greater than second
isParGT :: Edge -> Edge -> Bool
isParGT e e' = isParallel e e' && weightOf e > weightOf e'

--verify if two edges are parallel, if true, verify if first is lower than second
isParLT :: Edge -> Edge -> Bool
isParLT e e' = isParallel e e' && weightOf e < weightOf e'
