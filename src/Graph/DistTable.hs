module Graph.DistTable where

import Data.List
import Data.Maybe
import Graph.Digraph

type Dist = (NodeId, Maybe Float)

distTable :: Digraph -> [Dist]
distTable digraph = allDist [] (getNodes digraph) [] digraph



allDist :: [NodeId] -> [NodeId] -> [Dist] -> Digraph -> [Dist]

allDist  _      []   dist  _   = dist

allDist  []    open   _    g   = allDist [0] open' dist' g
  where
    open' = (filter (0/=) open)
    dist' = (0,Just 0): map (\x -> (x,Nothing)) open'



allDist close  open  dist  g   = do
  let d = sort $ newDist close g dist

      mo = minimumOpen open d    

      minimumOpen :: [NodeId] -> [Dist] -> Maybe NodeId
      minimumOpen open   []   = Nothing
      minimumOpen open (d:ds) = isOpen d
        where
          isOpen (x,Just y)  = if elem x open then Just x else minimumOpen open ds
          isOpen (x,Nothing) = minimumOpen open ds



  
      newDist :: [NodeId] -> Digraph -> [Dist] -> [Dist]
      newDist  []   _ dist = []

      newDist close g dist = do
        let new = concat $ map lambda close
              where
                lambda = (\x -> distNeighbours x (lk x) g)
                lk x = fromJust $ lookup x dist

            refreshDist :: [Dist] -> [Dist] -> [Dist]
            refreshDist []    old = old
            refreshDist new   []  = new
            refreshDist ((n,Just w):ns) old = refreshDist ns old'
              where
                old' = map refr old
                refr (x,Just y)  = if n==x then (x, Just (min w y))
                                   else (x,Just y)
                refr (x,Nothing) = if n==x then (x, Just w)
                                   else (x,Nothing)

            distNeighbours :: NodeId -> Maybe Float -> Digraph -> [Dist]
            distNeighbours n (Just d) digraph = map lambda $ archsFromNode n digraph
              where
                lambda = (\x -> (targetOf x, Just (d + weightOf x)))
          
        refreshDist new dist

  case mo of
    Nothing -> d
    Just x  -> allDist (x:close) (filter (x/=) open) d g
