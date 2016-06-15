module Graph.Parse (
  txtDigraph,
  digraphToGraph
  ) where

import           Data.List
import           Graph.Digraph
import qualified Graph.Graph as G

txtDigraph :: String -> Digraph
txtDigraph txt= do 
  let --Parser for archs of digraph
    parseArchs :: [String] -> Digraph -> Int -> Digraph
    parseArchs []     digraph   _  = digraph
    parseArchs (h:ts) digraph l    = if null h then digraph else parseArchs ts g (l-1)
      where
        arch = words h
        s = read $ arch !! 0 :: Int
        t = read $ arch !! 1 :: Int
        w = read $ arch !! 2 :: Float   
        g = newArch (l,s,t,w) digraph

    --Spliting file in relevant binds
    (n:(e:archs)) = lines txt
  
    --Parsing nodes of digraph
    digraph = buildDigraph [0..(read n :: Int)-1] []

    --Parsing archs of digraph
    digraph' = parseArchs archs digraph (read e :: Int)
      
  digraph'

digraphToGraph :: Digraph -> G.Graph
digraphToGraph digraph = graph'
  where
    graph = G.buildGraph (getNodes digraph) []
    allEs = archToEdges  (getArchs digraph) graph
    graph' = G.removeParallel allEs
    
    archToEdges :: [Arch] -> G.Graph -> G.Graph
    archToEdges [] g = g
    archToEdges (a:as) g = G.newEdge (l,s,t,w) (archToEdges as g)
      where
        l = labelOf a
        s = sourceOf a
        t = targetOf a
        w = weightOf a

