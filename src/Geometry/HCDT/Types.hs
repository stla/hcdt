{-# LANGUAGE InstanceSigs #-}

module Geometry.HCDT.Types
  where
import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.IntMap.Strict (IntMap)
import           Data.List          (sort)

data Triangle = Triangle Int Int Int 
  deriving (Show, Read)
instance Eq Triangle where
    (==) :: Triangle -> Triangle -> Bool
    Triangle i j k == Triangle i' j' k' = sort [i, j, k] == sort [i', j', k']

data Edge = Edge Int Int
  deriving (Show, Read)
instance Eq Edge where
    (==) :: Edge -> Edge -> Bool
    Edge i j == Edge i' j' = (i == i' && j == j') || (i == j' && j == i')
instance Hashable Edge where
  hashWithSalt s (Edge m1 m2) = hashWithSalt s (min m1 m2) `hashWithSalt` (max m1 m2)

data Vertex = Vertex Double Double
  deriving (Show, Eq)

data Triangulation = Triangulation {
    _vertices  :: IntMap Vertex
  , _triangles :: [Triangle]
  , _edges     :: [Edge]
} deriving Show

data ConstrainedTriangulation = ConstrainedTriangulation {
    _triangulation :: Triangulation
  , _fixedEdges    :: [Edge]
} deriving Show