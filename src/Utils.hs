module Utils
  ( borderEdges
  )
where
    
import           Types 
import           Data.Sequence as DS (Seq, fromList, (><), filter) 

triangleEdges :: Triangle -> Seq Edge
triangleEdges (Triangle i j k) = fromList [Edge i j, Edge j k, Edge i k]

isUnique :: Eq a => Seq a -> a -> Bool
isUnique xs x = length (DS.filter (== x) xs) == 1

allEdges :: Triangulation -> Seq Edge
allEdges triangulation = 
  let triplets = map triangleEdges (_triangles triangulation) in
    foldl1 (><) triplets

borderEdges :: Triangulation -> Seq Edge
borderEdges triangulation = DS.filter (isUnique edges) edges
  where
    edges = allEdges triangulation

