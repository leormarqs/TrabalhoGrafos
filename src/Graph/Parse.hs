module Graph.Parse where

import Graph.Graph

parseGraph :: String -> IO Graph
parseGraph filePath = do
  --Reading text file
  txt <- readFile filePath
  
  let --Parser for edges of graph
      parseEdges :: [String] -> Graph -> Int -> Graph
      parseEdges [] graph _ = graph
      parseEdges (h:ts) graph l = parseEdges ts g (l+1)
        where
          edge = words h
          s = read $ edge !! 0 :: Int
          t = read $ edge !! 1 :: Int
          v = read $ edge !! 2 :: Float   
          g = insertEdge (l,s,t,v) graph

      --Spliting file in relevant binds
      (n:(e:edges)) = lines txt
  
      --Parsing nodes of graph
      graph = buildGraph [0..(read n :: Int)-1] []

      --Parsing edges of graph
      graph' = parseEdges edges graph 0
      
  return graph'
