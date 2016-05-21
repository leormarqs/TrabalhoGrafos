module Main where

import Graph.Parse
import Graph.Digraph
import Data.List

main :: IO ()
main = do
  txt <- readFile "./src/Graph/grafo.txt"
  let dg = txtDigraph txt
      g  = digraphToGraph dg
  print dg
  print g
