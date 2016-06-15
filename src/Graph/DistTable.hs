{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graph.DistTable (
  Dist,
  distTable
  ) where

import Data.List
import Data.Maybe
import Graph.Digraph

---------------------------------------------------------------------------

--Distance Table Implementation
type Dist = (NodeId, Maybe Float)

--Distance Table visualization
instance {-# OVERLAPPING #-} Show Dist where
  show (a,Just b) = " | d(0," ++ show a ++ ") | " ++ show b ++ " |\n"
  show (a,Nothing)= " | d(0," ++ show a ++ ") |  ∞  |\n"

---------------------------------------------------------------------------

--Initialize Distance Table Calc
distTable :: Digraph -> [Dist]
distTable digraph = allDist [] (getNodes digraph) [] digraph



--Implements the Dijkstra Algorithm, starting from node 0.
allDist :: [NodeId] -> [NodeId] -> [Dist] -> Digraph -> [Dist]
allDist  _      []   dist  _   = dist

--Initialize DistTable with "infinity" values
allDist  []    open   _    g   = allDist [0] open' dist' g
  where
    open' = (filter (0/=) open)
    dist' = (0,Just 0): map (\x -> (x,Nothing)) open'

--Calculate the dist table
allDist close  open  dist  g   = do
  let
    --Find the minimum dist not closed yet
    --DistTable must be ordered
    minimumOpen :: [NodeId] -> [Dist] -> Maybe NodeId
    minimumOpen open   []   = Nothing
    minimumOpen open (d:ds) = isOpen d
        where
          isOpen (x,Just y)  = if elem x open then Just x else minimumOpen open ds
          isOpen (x,Nothing) = minimumOpen open ds

    --Refresh the distance table if some lower distance was found
    refreshDist :: [Dist] -> [Dist] -> [Dist]
    refreshDist []    old = old
    refreshDist new   []  = new
    refreshDist ((n,Just w):ns) old = refreshDist ns old'
      where
        old' = map lambda old
        lambda (x,Just y)  = if n==x
                             then (x, Just (min w y))
                             else (x,Just y)
        lambda (x,Nothing) = if n==x
                             then (x, Just w)
                             else (x,Nothing)

    --Get distances to a all neighbour from a specific node
    distNeighbours :: NodeId -> Maybe Float -> Digraph -> [Dist]
    distNeighbours n (Just d) digraph = map lambda $ archsFromNode n digraph
      where
        lambda = (\x -> (targetOf x, Just (d + weightOf x)))

    --Find new distances to refresh the distance table 
    newDist :: [NodeId] -> Digraph -> [Dist] -> [Dist]
    newDist  []   _ dist = []
    newDist close g dist = refreshDist new dist
      where
        new = concat $ map (\x -> distNeighbours x (lkup x) g) close
        lkup x = fromJust $ lookup x dist   


    --Find the new distances
    d = sort $ newDist close g dist
    --Find the node with minimum distance not closed
    mo = minimumOpen open d    

  case mo of
    --All connected nodes are closed, return the final table
    Nothing -> d
    --A connected node is open
    --close this node, and refresh the dist table
    Just x  -> allDist (x:close) (filter (x/=) open) d g
