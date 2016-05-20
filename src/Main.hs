module Main where

import Graph.Parse
import Graph.Digraph
import Data.List

main :: IO ()
main = do
  txt <- readFile "./src/Graph/grafo.txt"
  let g = txtDigraph txt
      a = archs g
  print $ sort a
