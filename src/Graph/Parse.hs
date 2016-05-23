module Graph.Parse where

import           Data.List
import           Graph.Digraph
import qualified Graph.Graph as G

txtDigraph :: String -> Digraph
txtDigraph txt= do 
  let --Parser for archs of digraph
    parseArchs :: [String] -> Digraph -> Int -> Digraph
    parseArchs [] digraph _ = digraph
    parseArchs (h:ts) digraph l = parseArchs ts g (l+1)
      where
        arch = words h
        s = read $ arch !! 0 :: Int
        t = read $ arch !! 1 :: Int
        v = read $ arch !! 2 :: Float   
        g = insertArch (l,s,t,v) digraph

    --Spliting file in relevant binds
    (n:(e:archs)) = lines txt
  
    --Parsing nodes of digraph
    digraph = buildDigraph [0..(read n :: Int)-1] []

    --Parsing archs of digraph
    digraph' = parseArchs archs digraph 0
      
  digraph'

digraphToGraph :: Digraph -> G.Graph
digraphToGraph digraph = graph'
  where
    graph = G.buildGraph (getNodes digraph) []
    allEs = archToEdges  (getArchs digraph) graph
    graph' = G.removeParallel allEs
    
    archToEdges :: [Arch] -> G.Graph -> G.Graph
    archToEdges [] g = g
    archToEdges ((Arch l s t v):ts) g = G.newEdge (l,s,t,v) (archToEdges ts g)


