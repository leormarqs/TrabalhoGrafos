module Main where

import Graph.Parse
import Graph.Graph

main :: IO ()
main = do
  g <- parseGraph "./src/Graph/grafo.txt"
  print g
