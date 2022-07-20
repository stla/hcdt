module Delaunay
  ( delaunay,
  )
where

import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           Triangulation
import           Types                 (Triangulation, Vertex)

delaunay :: [Vertex] -> IO Triangulation
delaunay vertices = do
  let nvertices = length vertices
  verticesPtr <- mallocBytes (nvertices * sizeOf (undefined :: CVertex))
  cvertices <- mapM vertexToCVertex vertices
  pokeArray verticesPtr cvertices
  ctriangulationPtr <- c_delaunay verticesPtr (fromIntegral nvertices)
  ctriangulation <- peek ctriangulationPtr 
  free verticesPtr
  free ctriangulationPtr
  cTriangulationToTriangulation ctriangulation

