module Graph.Graph where
{-

  TO DO 

  -- ** Delete/Update
  , removeEdge
  , removeNode
  
  -- ** extraction
  , incidentEdges
  , neighbourNodes

  -- ** Predicates
  , isAdjacentTo
  , isIncidentTo

-}

import Data.Set

--Ids of nodes and edges abstraction
type NodeId = Int
type EdgeId = Int

---------------------------------------------------------------------------------------

--Data type for edges
data Edge = Edge {
                    lbl :: EdgeId,
                    nds :: Set NodeId,
                    val :: Float
                  } deriving (Eq)

--Exibition of edges
instance Show Edge where
  show (Edge l n v) = "(" ++ show v ++ ")"

--Ordering of edges
instance Ord Edge where
  compare (Edge l n v) (Edge l' n' v') = compare (v,n) (v',n')

---------------------------------------------------------------------------------------

--Data type for Graphs
data Graph = Graph {
                      nodes :: [NodeId],
                      edges :: [Edge]
                    } deriving (Eq)

--Exibition of Graphs
instance Show Graph where
  show (Graph n e) = "Graph:\nNodes: " ++ show n ++ "\nEdges: " ++ show e

---------------------------------------------------------------------------------------

--Build a empty graph
emptyGraph :: Graph
emptyGraph = Graph [] []

--build a graph with pre-instantiated nodes and/or edges
buildGraph :: [NodeId] -> [(EdgeId, NodeId, NodeId,  Float)] -> Graph
buildGraph [] _ = error "Nodes list empty."
buildGraph n e  = Graph n (buildEdges e)
  where
    buildEdges :: [(EdgeId, NodeId, NodeId, Float)] -> [Edge]
    buildEdges [] = []
    buildEdges ((l,s,t,v):ts) = Edge l nds v : buildEdges ts
      where
        nds = insert s $ insert t empty

--insert a node in a graph
insertNode :: NodeId -> Graph -> Graph
insertNode n graph =
  if elem n ns then graph else Graph (n:ns) es
  where
    ns = nodes graph
    es = edges graph

--insert a edge in a graph
insertEdge :: (EdgeId, NodeId, NodeId, Float) -> Graph -> Graph
insertEdge (l,s,t,v) graph =
  if elem e es then graph else Graph ns (e:es)
  where
    ns  = nodes graph
    es  = edges graph
    e   = Edge l nds v
    nds = insert s $ insert t empty
