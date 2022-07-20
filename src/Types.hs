module Types
  where
import           Data.IntMap.Strict (IntMap)
import           Data.List          (sort)

data Triangle = Triangle Int Int Int 
  deriving (Show, Read)
instance Eq Triangle where
    Triangle i j k == Triangle i' j' k' = sort [i, j, k] == sort [i', j', k']

data Edge = Edge Int Int
  deriving (Show, Read)
instance Eq Edge where
    Edge i j == Edge i' j' = (i == i' && j == j') || (i == j' && j == i')

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