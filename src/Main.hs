module Main where

import Data.List
import Graph.Parse
import Graph.Digraph
import Graph.SpanningTree


main :: IO ()
main = do
  txt <- readFile "/home/lmrodrigues/Documents/TrabalhoGrafos/src/Graph/grafo.txt"
  let dg = txtDigraph txt
      g  = digraphToGraph dg
      mst = spanningTree g

  print dg
  print g
  print mst
