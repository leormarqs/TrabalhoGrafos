module Graph.Graph where

---------------------------------------------------------------------------------------

import qualified Data.Set as S

---------------------------------------------------------------------------------------

--Ids of nodes and edges abstraction
type NodeId = Int
type EdgeId = Int

--Data type for edges
data Edge = Edge {
                    lbl :: EdgeId,
                    nds :: S.Set NodeId,
                    val :: Float
                  }

--Exibition of edges
instance Show Edge where
  show (Edge l n v) = show (S.elems n,v)

--Order of edges
instance Ord Edge where
  compare (Edge l n v) (Edge l' n' v') = compare (v,n) (v',n')

--Equality of Edges
instance Eq Edge where
  (==) (Edge _ n v) (Edge _ n' v') = n == n' && v == v'

---------------------------------------------------------------------------------------

--Data type for Graphs
data Graph = Graph {
                      nodes :: S.Set NodeId,
                      edges :: S.Set Edge
                    } deriving (Eq)

--Exibition of Graphs
instance Show Graph where
  show (Graph n e) = "Graph:\nNodes: " ++ show (S.elems n) ++ "\nEdges: " ++ show (S.elems e)

---------------------------------------------------------------------------------------

--Build a empty graph
emptyGraph :: Graph
emptyGraph = Graph S.empty S.empty

--build a graph with pre-instantiated nodes and/or edges
buildGraph :: [NodeId] -> [(EdgeId, NodeId, NodeId,  Float)] -> Graph
buildGraph [] _ = error "Nodes list empty."
buildGraph n e  = Graph (S.fromList n) (buildEdges e)
  where
    buildEdges :: [(EdgeId, NodeId, NodeId, Float)] -> S.Set Edge
    buildEdges [] = S.empty
    buildEdges ((l,s,t,v):ts) = S.insert (Edge l nds v) (buildEdges ts)
      where
        nds = S.insert s $ S.insert t S.empty

--insert a node in a graph
insertNode :: NodeId -> Graph -> Graph
insertNode n graph =
  if elem n ns then graph else Graph (S.insert n ns) es
  where
    ns = nodes graph
    es = edges graph

--insert a edge in a graph
insertEdge :: (EdgeId, NodeId, NodeId, Float) -> Graph -> Graph
insertEdge (l,s,t,v) graph =
  if elem e es then graph else Graph ns (S.insert e es)
  where
    ns  = nodes graph
    es  = edges graph
    e   = Edge l nds v
    nds = S.insert s $ S.insert t S.empty

--remove a node and edges connected to it from a graph
removeNode :: NodeId -> Graph -> Graph
removeNode n (Graph ns es) = Graph ns' es'
  where
    ns' = S.filter (n /=) ns
    es' = S.filter (\x -> not $ S.member n (nds x)) es

--remove a edge from a graph
removeEdge :: EdgeId -> Graph -> Graph
removeEdge e (Graph ns es) = Graph ns es'
  where
    es' = S.filter (\x -> e /= lbl x) es

---------------------------------------------------------------------------------------

--find all incident edges on a node
incidentEdges :: NodeId -> Graph -> S.Set Edge
incidentEdges n graph = S.filter (\x -> S.member n (nds x)) es
  where
    es = edges graph

--find all adjacents nodes from a node
neighbourNodes :: NodeId -> Graph -> S.Set NodeId
neighbourNodes n graph = S.delete n $ S.fromList ns
  where
    es = incidentEdges n graph
    ns = concat . S.elems $ S.map (\x-> S.elems $ nds x) es

---------------------------------------------------------------------------------------

--verify if two nodes are adjacents
isAdjacentTo :: NodeId -> NodeId -> Graph -> Bool
isAdjacentTo n n' graph = S.member n' $ neighbourNodes n graph

--verify if a edge is adjacent to a node
isIncidentTo :: Edge -> NodeId -> Bool
isIncidentTo e n = S.member n $ nds e

--Verify if two edges are parallels
isParallel :: Edge -> Edge -> Bool
isParallel (Edge _ n _) (Edge _ n' _)  = n == n'

--Verify if two edges aren't parallels
notParallel :: Edge -> Edge -> Bool
notParallel e e' = (not $ isParallel e e')

--verify if two edges are parallel, if true, verify if first is greater than second
isParGT :: Edge -> Edge -> Bool
isParGT e e' = isParallel e e' && val e > val e'

--verify if two edges are parallel, if true, verify if first is lower than second
isParLT :: Edge -> Edge -> Bool
isParLT e e' = isParallel e e' && val e < val e'
