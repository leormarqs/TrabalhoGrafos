module Graph.Digraph where

{-

  TO DO 

  -- ** Delete/Update
  , removeArch
  , removeNode
  
  -- ** extraction
  , archsFromNode
  , archsIntoNode
  , incidentArchs
  , neighbourNodes
  , nodesConnectedTo
    
  , sourceOf
  , targetOf
    
  -- ** Predicates
  , isArchOf
  , isNodeOf
  , isAdjacentTo
  , isIncidentTo

-}

--Ids of nodes and archs abstraction
type NodeId = Int
type ArchId = Int

---------------------------------------------------------------------------------------

--Data type for archs
data Arch = Arch {
                    lbl :: ArchId,
                    src :: NodeId,
                    tgt :: NodeId,
                    val :: Float
                  } deriving (Eq)

--Exibition of archs
instance Show Arch where
  show (Arch n s t v) = " " ++ show s ++ " -(" ++ show v ++ ")-> " ++ show t ++ " "

--Ordering of archs
instance Ord Arch where
  compare (Arch l s t v) (Arch l' s' t' v') = compare (v,s,t) (v',s',t')

---------------------------------------------------------------------------------------

--Data type for Digraphs
data Digraph = Digraph {
                      nodes :: [NodeId],
                      archs :: [Arch]
                    } deriving (Eq)

--Exibition of Digraphs
instance Show Digraph where
  show (Digraph n a) = "Digraph:\nNodes: " ++ show n ++ "\nArchs: " ++ show a

---------------------------------------------------------------------------------------

--Build a empty graph
emptyDigraph :: Digraph
emptyDigraph = Digraph [] []

--build a graph with pre-instantiated nodes and/or archs
buildDigraph :: [NodeId] -> [(ArchId, NodeId, NodeId, Float)] -> Digraph
buildDigraph [] _ = error "Nodes list empty."
buildDigraph n e  = Digraph n (buildArchs e)
  where
    buildArchs :: [(ArchId, NodeId, NodeId, Float)] -> [Arch]
    buildArchs [] = []
    buildArchs ((l,s,t,v):ts) = Arch l s t v : buildArchs ts

--insert a node in a graph
insertNode :: NodeId -> Digraph -> Digraph
insertNode n graph =
  if elem n ns then graph else Digraph (n:ns) es
  where
    ns = nodes graph
    es = archs graph

--insert an arch in a graph
insertArch :: (ArchId,NodeId,NodeId,Float) -> Digraph -> Digraph
insertArch (l,s,t,v) graph =
  if elem a as then graph else Digraph ns (a:as)
  where
    ns = nodes graph
    as = archs graph
    a  = Arch l s t v
