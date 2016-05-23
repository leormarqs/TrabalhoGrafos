module Graph.Digraph where

---------------------------------------------------------------------------------------

import Data.List

---------------------------------------------------------------------------------------

--Ids of nodes and archs abstraction
type NodeId = Int
type ArchId = Int


--Data type for archs
data Arch = Arch {
                    label  :: ArchId,
                    source :: NodeId,
                    target :: NodeId,
                    weight :: Float
                  }

--Exibition of archs
instance Show Arch where
  show (Arch n s t v) = " " ++ show s ++ " -(" ++ show v ++ ")-> " ++ show t ++ " "

--Ordering of archs
instance Ord Arch where
  compare (Arch l s t v) (Arch l' s' t' v') = compare (v,s,t) (v',s',t')

instance Eq Arch where
  (Arch _ s t v) == (Arch _ s' t' v') = (s,t,v) == (s',t',v')


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
    ns = getNodes graph
    es = getArchs graph

--insert an existant arch in a graph
insertArch :: Arch -> Digraph -> Digraph
insertArch a graph =
  if elem a as then graph else Digraph ns (sort $ a:as)
  where
    ns = getNodes graph
    as = getArchs graph
    
--insert a new arch in a graph
newArch :: (ArchId, NodeId, NodeId, Float) -> Digraph -> Digraph
newArch (l,s,t,w) graph =
  if elem e es then graph else Digraph ns (sort $ e:es)
  where
    ns  = getNodes graph
    es  = getArchs graph
    e   = Arch l s t w

--remove a node and the archs connected to it
removeNode :: NodeId -> Digraph -> Digraph
removeNode n (Digraph ns a) = Digraph ns' a'
  where
    ns' = delete n ns
    a'  = filter (\x -> n /= sourceOf x) a''
    a'' = filter (\x -> n /= targetOf x) a
    
--remove a Arch from a digraph
removeArch :: ArchId -> Digraph -> Digraph
removeArch a (Digraph n as) = Digraph n as'
  where
    as' = filter (\x -> a /= labelOf x) as

---------------------------------------------------------------------------------------
getNodes :: Digraph -> [NodeId]
getNodes (Digraph [] _) = error "Empty nodes Digraph"
getNodes (Digraph n  _)  = sort n

getArchs :: Digraph -> [Arch]
getArchs (Digraph _ a) = sort a

--get the label of a arch
labelOf :: Arch -> ArchId
labelOf (Arch l _ _ _) = l

--get the source of a arch
sourceOf :: Arch -> NodeId
sourceOf (Arch _ s _ _) = s

--get the target of a arch
targetOf :: Arch -> NodeId
targetOf (Arch _ _ t _) = t

--get the weight of a arch
weightOf :: Arch -> Float
weightOf (Arch _ _ _ w) = w 

--get the archs out of n
archsFromNode :: NodeId -> Digraph -> [Arch]
archsFromNode n (Digraph _ as) = sort $ filter (\x -> n == sourceOf x) as

--get the archs in n
archsIntoNode :: NodeId -> Digraph -> [Arch]
archsIntoNode n (Digraph _ as) = sort $ filter (\x -> n == targetOf x) as

--get the archs in or out on n
incidentArchs :: NodeId -> Digraph -> [Arch]
incidentArchs n (Digraph _ as) = sort $ filter (\x -> n == sourceOf x || n == sourceOf x) as

--get the adjacent nodes from n
neighbourNodes :: NodeId -> Digraph -> [NodeId]
neighbourNodes n g = sort $ map targetOf (archsFromNode n g)

---------------------------------------------------------------------------------------

--verify if two nodes are adjacents
isAdjacentTo :: NodeId -> NodeId -> Digraph -> Bool
isAdjacentTo n n' graph = elem n' $ neighbourNodes n graph

--verify if a edge is adjacent to a node
isIncidentTo :: Arch -> NodeId -> Bool
isIncidentTo e n = targetOf e == n

