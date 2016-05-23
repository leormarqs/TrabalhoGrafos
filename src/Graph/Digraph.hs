module Graph.Digraph where

---------------------------------------------------------------------------------------

import Data.List

---------------------------------------------------------------------------------------

--Ids of nodes and archs abstraction
type NodeId = Int
type ArchId = Int


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
buildDigraph n e  = Digraph (sort n) (sort $ buildArchs e)
  where
    buildArchs :: [(ArchId, NodeId, NodeId, Float)] -> [Arch]
    buildArchs [] = []
    buildArchs ((l,s,t,v):ts) = (Arch l s t v) : (buildArchs ts)

--insert a node in a graph
insertNode :: NodeId -> Digraph -> Digraph
insertNode n graph =
  if elem n ns then graph else Digraph (sort $ n:ns) es
  where
    ns = nodes graph
    es = archs graph

--insert an arch in a graph
insertArch :: (ArchId,NodeId,NodeId,Float) -> Digraph -> Digraph
insertArch (l,s,t,v) graph =
  if elem a as then graph else Digraph ns (sort $ a:as)
  where
    ns = nodes graph
    as = archs graph
    a  = Arch l s t v

--remove a Arch from a digraph
removeArch :: ArchId -> Digraph -> Digraph
removeArch a (Digraph n as) = Digraph n as'
  where
    as' = filter (\x -> a /= lbl x) as

--remove a node and the archs connected to it
removeNode :: NodeId -> Digraph -> Digraph
removeNode n (Digraph ns a) = Digraph ns' a'
  where
    ns' = delete n ns
    a'  = filter (\x -> n /= sourceOf x) a''
    a'' = filter (\x -> n /= targetOf x) a

---------------------------------------------------------------------------------------
getNodes :: Digraph -> [NodeId]
getNodes g = sort $ nodes g

getArchs :: Digraph -> [Arch]
getArchs g = sort $ archs g

--get the source of a arch
sourceOf :: Arch -> NodeId
sourceOf a = src a

--get the target of a arch
targetOf :: Arch -> NodeId
targetOf a = tgt a

--get the archs out of n
archsFromNode :: NodeId -> Digraph -> [Arch]
archsFromNode n (Digraph _ as) = sort $ filter (\x -> n == src x) as

--get the archs in n
archsIntoNode :: NodeId -> Digraph -> [Arch]
archsIntoNode n (Digraph _ as) = sort $ filter (\x -> n == tgt x) as

--get the archs in or out in n
incidentArchs :: NodeId -> Digraph -> [Arch]
incidentArchs n (Digraph _ as) = sort $ filter (\x -> n == src x || n == tgt x) as

--get the adjacent nodes from n
neighbourNodes :: NodeId -> Digraph -> [NodeId]
neighbourNodes n g = sort $ map tgt (archsFromNode n g)

---------------------------------------------------------------------------------------

--verify if two nodes are adjacents
isAdjacentTo :: NodeId -> NodeId -> Digraph -> Bool
isAdjacentTo n n' graph = elem n' $ neighbourNodes n graph

--verify if a edge is adjacent to a node
isIncidentTo :: Arch -> NodeId -> Bool
isIncidentTo e n = targetOf e == n

