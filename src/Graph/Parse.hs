module Graph.Parse where

import Graph.Digraph

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
