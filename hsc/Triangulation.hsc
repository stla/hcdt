{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Triangulation
( cTriangulationToTriangulation
, vertexToCVertex
, c_delaunay 
, CTriangulation (..)
, CVertex (..))
  where
import           Types
import           Foreign
import           Foreign.C.Types
import qualified Data.IntMap.Strict as IM

#include "hcdt.hpp"

data CVertex = CVertex {
    __x :: CDouble
  , __y :: CDouble
}

instance Storable CVertex where
    sizeOf    __ = #{size VertexT}
    alignment __ = #{alignment VertexT}
    peek ptr = do
      x'  <- #{peek VertexT, x} ptr
      y'  <- #{peek VertexT, y} ptr
      return CVertex { __x = x', __y = y' }
    poke ptr (CVertex r1 r2)
      = do
        #{poke VertexT, x} ptr r1
        #{poke VertexT, y} ptr r2

cVertexToVertex :: CVertex -> IO Vertex
cVertexToVertex cvertex = do
  let x = realToFrac $ __x cvertex
  let y = realToFrac $ __y cvertex
  return $ Vertex x y

vertexToCVertex :: Vertex -> IO CVertex
vertexToCVertex (Vertex x y) = do
  return $ CVertex { __x = realToFrac x, __y = realToFrac y }

data CEdge = CEdge {
    __i :: CUInt
  , __j :: CUInt
}

instance Storable CEdge where
    sizeOf    __ = #{size EdgeT}
    alignment __ = #{alignment EdgeT}
    peek ptr = do
      i'  <- #{peek EdgeT, i} ptr
      j'  <- #{peek EdgeT, j} ptr
      return CEdge { __i = i'
                   , __j = j' }
    poke ptr (CEdge r1 r2)
      = do
        #{poke EdgeT, i} ptr r1
        #{poke EdgeT, j} ptr r2

cEdgeToEdge :: CEdge -> IO Edge
cEdgeToEdge cedge = do
  let i = fromIntegral $ __i cedge
  let j = fromIntegral $ __j cedge
  return $ Edge i j

data CTriangle = CTriangle {
    __i1 :: CUInt
  , __i2 :: CUInt
  , __i3 :: CUInt
}

instance Storable CTriangle where
    sizeOf    __ = #{size TriangleT}
    alignment __ = #{alignment TriangleT}
    peek ptr = do
      i1'  <- #{peek TriangleT, i1} ptr
      i2'  <- #{peek TriangleT, i2} ptr
      i3'  <- #{peek TriangleT, i3} ptr
      return CTriangle { __i1 = i1'
                       , __i2 = i2'
                       , __i3 = i3' }
    poke ptr (CTriangle r1 r2 r3)
      = do
        #{poke TriangleT, i1} ptr r1
        #{poke TriangleT, i2} ptr r2
        #{poke TriangleT, i3} ptr r3

cTriangleToTriangle :: CTriangle -> IO Triangle
cTriangleToTriangle ctriangle = do
  let i1 = fromIntegral $ __i1 ctriangle
  let i2 = fromIntegral $ __i2 ctriangle
  let i3 = fromIntegral $ __i3 ctriangle
  return $ Triangle i1 i2 i3

data CTriangulation = CTriangulation {
    __vertices   :: Ptr CVertex
  , __nvertices  :: CSize
  , __triangles  :: Ptr CTriangle
  , __ntriangles :: CSize
  , __edges      :: Ptr CEdge
  , __nedges     :: CSize
}

instance Storable CTriangulation where
    sizeOf    __ = #{size TriangulationT}
    alignment __ = #{alignment TriangulationT}
    peek ptr = do
      vs  <- #{peek TriangulationT, vertices} ptr
      nvs <- #{peek TriangulationT, nvertices} ptr
      ts  <- #{peek TriangulationT, triangles} ptr
      nts <- #{peek TriangulationT, ntriangles} ptr
      es  <- #{peek TriangulationT, edges} ptr
      nes <- #{peek TriangulationT, nedges} ptr
      return CTriangulation { __vertices   = vs
                            , __nvertices  = nvs
                            , __triangles  = ts
                            , __ntriangles = nts
                            , __edges      = es
                            , __nedges     = nes }
    poke ptr (CTriangulation r1 r2 r3 r4 r5 r6)
      = do
        #{poke TriangulationT, vertices}   ptr r1
        #{poke TriangulationT, nvertices}  ptr r2
        #{poke TriangulationT, triangles}  ptr r3
        #{poke TriangulationT, ntriangles} ptr r4
        #{poke TriangulationT, edges}      ptr r5
        #{poke TriangulationT, nedges}     ptr r6

cTriangulationToTriangulation :: CTriangulation -> IO Triangulation
cTriangulationToTriangulation ctriangulation = do
  let nvertices  = fromIntegral $ __nvertices ctriangulation
      ntriangles = fromIntegral $ __ntriangles ctriangulation
      nedges     = fromIntegral $ __nedges ctriangulation
  vertices  <- peekArray nvertices  (__vertices ctriangulation)
  triangles <- peekArray ntriangles (__triangles ctriangulation)
  edges     <- peekArray nedges     (__edges ctriangulation)
  vertices'  <- mapM cVertexToVertex vertices
  triangles' <- mapM cTriangleToTriangle triangles
  edges'     <- mapM cEdgeToEdge edges
  return $ Triangulation { _vertices  = IM.fromAscList (zip [0 .. nvertices-1] vertices')
                         , _triangles = triangles'
                         , _edges     = edges' }

foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CVertex -> CSize -> IO (Ptr CTriangulation)
