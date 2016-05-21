module Graph.Digraph where

---------------------------------------------------------------------------------------

import qualified Data.Set as S

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
                      nodes :: S.Set NodeId,
                      archs :: S.Set Arch
                    } deriving (Eq)

--Exibition of Digraphs
instance Show Digraph where
  show (Digraph n a) = "Digraph:\nNodes: " ++ show n ++ "\nArchs: " ++ show a

---------------------------------------------------------------------------------------

--Build a empty graph
emptyDigraph :: Digraph
emptyDigraph = Digraph S.empty S.empty

--build a graph with pre-instantiated nodes and/or archs
buildDigraph :: [NodeId] -> [(ArchId, NodeId, NodeId, Float)] -> Digraph
buildDigraph [] _ = error "Nodes list empty."
buildDigraph n e  = Digraph (S.fromList n) (buildArchs e)
  where
    buildArchs :: [(ArchId, NodeId, NodeId, Float)] -> S.Set Arch
    buildArchs [] = S.empty
    buildArchs ((l,s,t,v):ts) = S.insert (Arch l s t v) (buildArchs ts)

--insert a node in a graph
insertNode :: NodeId -> Digraph -> Digraph
insertNode n graph =
  if elem n ns then graph else Digraph (S.insert n ns) es
  where
    ns = nodes graph
    es = archs graph

--insert an arch in a graph
insertArch :: (ArchId,NodeId,NodeId,Float) -> Digraph -> Digraph
insertArch (l,s,t,v) graph =
  if elem a as then graph else Digraph ns (S.insert a as)
  where
    ns = nodes graph
    as = archs graph
    a  = Arch l s t v

--remove a Arch from a digraph
removeArch :: ArchId -> Digraph -> Digraph
removeArch a (Digraph n as) = Digraph n as'
  where
    as' = S.filter (\x -> a /= lbl x) as

--remove a node and the archs connected to it
removeNode :: NodeId -> Digraph -> Digraph
removeNode n (Digraph ns a) = Digraph ns' a'
  where
    ns' = S.delete n ns
    a'  = S.filter (\x -> n /= src x) a''
    a'' = S.filter (\x -> n /= tgt x) a

---------------------------------------------------------------------------------------

--get the source of a arch
sourceOf :: Arch -> NodeId
sourceOf a = src a

--get the target of a arch
targetOf :: Arch -> NodeId
targetOf a = tgt a

--get the archs out of n
archsFromNode :: NodeId -> Digraph -> S.Set Arch
archsFromNode n (Digraph _ as) = S.filter (\x -> n == src x) as

--get the archs in n
archsIntoNode :: NodeId -> Digraph -> S.Set Arch
archsIntoNode n (Digraph _ as) = S.filter (\x -> n == tgt x) as

--get the archs in or out in n
incidentArchs :: NodeId -> Digraph -> S.Set Arch
incidentArchs n (Digraph _ as) = S.filter (\x -> n == src x || n == tgt x) as

--get the adjacent nodes from n
neighbourNodes :: NodeId -> Digraph -> S.Set NodeId
neighbourNodes n g = S.map (\x -> tgt x) $ archsFromNode n g

---------------------------------------------------------------------------------------

--verify if two nodes are adjacents
isAdjacentTo :: NodeId -> NodeId -> Digraph -> Bool
isAdjacentTo n n' graph = S.member n' $ neighbourNodes n graph

--verify if a edge is adjacent to a node
isIncidentTo :: Arch -> NodeId -> Bool
isIncidentTo e n = targetOf e == n
