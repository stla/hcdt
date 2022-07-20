module Types
  where
import           Data.IntMap.Strict (IntMap)

cantorPairing :: (Int, Int) -> Int
cantorPairing (k1, k2) = (k1+k2)*(k1+k2+1) + 2*k2

cantorTripling :: (Int, Int, Int) -> Int
cantorTripling (k1, k2, k3) = cantorPairing(cantorPairing(k1, k2), k3)

data Triangle = Triangle Int Int Int 
  deriving (Show, Read)
instance Eq Triangle where
    Triangle i j k == Triangle i' j' k' = cantorTripling (i, j, k) == cantorTripling (i', j', k')

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