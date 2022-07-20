module Delaunay
  ( delaunay,
    cdelaunay
  )
where

import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           Triangulation
import           Types                 (Triangulation, Vertex, ConstrainedTriangulation, Edge)

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

cdelaunay :: [Vertex] -> [Edge] -> IO ConstrainedTriangulation
cdelaunay vertices edges = do
  let nvertices = length vertices
  verticesPtr <- mallocBytes (nvertices * sizeOf (undefined :: CVertex))
  cvertices <- mapM vertexToCVertex vertices
  pokeArray verticesPtr cvertices
  let nedges = length edges
  edgesPtr <- mallocBytes (nedges * sizeOf (undefined :: CEdge))
  cedges <- mapM edgeToCEdge edges
  pokeArray edgesPtr cedges
  cctriangulationPtr <- c_cdelaunay verticesPtr (fromIntegral nvertices) edgesPtr (fromIntegral nedges)
  cctriangulation <- peek cctriangulationPtr 
  free verticesPtr
  free edgesPtr
  free cctriangulationPtr
  cCTriangulationToConstrainedTriangulation cctriangulation
