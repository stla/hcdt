module Utils
  ( borderEdges
  , borderEdges2
  )
where
    
import           Types 
import           Data.Sequence as DS     (Seq, fromList, (><), filter, drop) 
import           Data.Maybe              (isNothing, fromJust)
import           Data.Foldable           (find)
import           Data.Foldable.WithIndex (ifind)

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

isUnique2 :: Eq a => Seq a -> a -> Bool
isUnique2 xs x = isNothing y 
  where
    (i, _) = fromJust $ ifind (\_ x' -> x' == x) xs
    y = find (== x) (DS.drop i xs)


borderEdges2 :: Triangulation -> Seq Edge
borderEdges2 triangulation = DS.filter (isUnique2 edges) edges
  where
    edges = allEdges triangulation