module Geometry.HCDT.Utils
  ( borderEdges,
    partitionEdges
  )
where
    
import Geometry.HCDT.Types
    ( Triangulation(_triangles), Edge(..), Triangle(..) ) 
import           Data.Sequence as DS     (Seq, fromList, (><), filter, deleteAt) 
import           Data.Maybe              (isNothing, fromJust)
import           Data.Foldable           (find)
import           Data.Foldable.WithIndex (ifind)
import           Witherable              (hashNub)

triangleEdges :: Triangle -> Seq Edge
triangleEdges (Triangle i j k) = fromList [Edge i j, Edge j k, Edge i k]

-- isUnique :: Eq a => Seq a -> a -> Bool
-- isUnique xs x = length (DS.filter (== x) xs) == 1

allEdges :: Triangulation -> Seq Edge
allEdges triangulation = 
  let triplets = map triangleEdges (_triangles triangulation) in
    foldl1 (><) triplets

-- borderEdges :: Triangulation -> Seq Edge
-- borderEdges triangulation = DS.filter (isUnique edges) edges
--   where
--     edges = allEdges triangulation

isUnique :: Eq a => Seq a -> a -> Bool
isUnique xs x = isNothing y 
  where
    (i, _) = fromJust $ ifind (\_ x' -> x' == x) xs
    y = find (== x) (DS.deleteAt i xs)

-- | Exterior edges of a Delaunay triangulation.
borderEdges :: Triangulation -> Seq Edge
borderEdges triangulation = DS.filter (isUnique edges) edges
  where
    edges = allEdges triangulation

-- | Exterior and interior edges of a Delaunay triangulation.
partitionEdges :: Triangulation -> (Seq Edge, Seq Edge)
partitionEdges triangulation = (exEdges, inEdges)
  where
    edges = allEdges triangulation
    exEdges = DS.filter (isUnique edges) edges
    inEdges = hashNub $ DS.filter (not . isUnique edges) edges