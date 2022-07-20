{-# LINE 1 "Triangulation.hsc" #-}
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



data CVertex = CVertex {
    __x :: CDouble
  , __y :: CDouble
}

instance Storable CVertex where
    sizeOf    __ = (16)
{-# LINE 24 "Triangulation.hsc" #-}
    alignment __ = 8
{-# LINE 25 "Triangulation.hsc" #-}
    peek ptr = do
      x'  <- (`peekByteOff` 0) ptr
{-# LINE 27 "Triangulation.hsc" #-}
      y'  <- (`peekByteOff` 8) ptr
{-# LINE 28 "Triangulation.hsc" #-}
      return CVertex { __x = x', __y = y' }
    poke ptr (CVertex r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 32 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 33 "Triangulation.hsc" #-}

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
    sizeOf    __ = (8)
{-# LINE 51 "Triangulation.hsc" #-}
    alignment __ = 4
{-# LINE 52 "Triangulation.hsc" #-}
    peek ptr = do
      i'  <- (`peekByteOff` 0) ptr
{-# LINE 54 "Triangulation.hsc" #-}
      j'  <- (`peekByteOff` 4) ptr
{-# LINE 55 "Triangulation.hsc" #-}
      return CEdge { __i = i'
                   , __j = j' }
    poke ptr (CEdge r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 60 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r2
{-# LINE 61 "Triangulation.hsc" #-}

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
    sizeOf    __ = (12)
{-# LINE 76 "Triangulation.hsc" #-}
    alignment __ = 4
{-# LINE 77 "Triangulation.hsc" #-}
    peek ptr = do
      i1'  <- (`peekByteOff` 0) ptr
{-# LINE 79 "Triangulation.hsc" #-}
      i2'  <- (`peekByteOff` 4) ptr
{-# LINE 80 "Triangulation.hsc" #-}
      i3'  <- (`peekByteOff` 8) ptr
{-# LINE 81 "Triangulation.hsc" #-}
      return CTriangle { __i1 = i1'
                       , __i2 = i2'
                       , __i3 = i3' }
    poke ptr (CTriangle r1 r2 r3)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 87 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r2
{-# LINE 88 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r3
{-# LINE 89 "Triangulation.hsc" #-}

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
    sizeOf    __ = (48)
{-# LINE 108 "Triangulation.hsc" #-}
    alignment __ = 8
{-# LINE 109 "Triangulation.hsc" #-}
    peek ptr = do
      vs  <- (`peekByteOff` 0) ptr
{-# LINE 111 "Triangulation.hsc" #-}
      nvs <- (`peekByteOff` 8) ptr
{-# LINE 112 "Triangulation.hsc" #-}
      ts  <- (`peekByteOff` 16) ptr
{-# LINE 113 "Triangulation.hsc" #-}
      nts <- (`peekByteOff` 24) ptr
{-# LINE 114 "Triangulation.hsc" #-}
      es  <- (`peekByteOff` 32) ptr
{-# LINE 115 "Triangulation.hsc" #-}
      nes <- (`peekByteOff` 40) ptr
{-# LINE 116 "Triangulation.hsc" #-}
      return CTriangulation { __vertices   = vs
                            , __nvertices  = nvs
                            , __triangles  = ts
                            , __ntriangles = nts
                            , __edges      = es
                            , __nedges     = nes }
    poke ptr (CTriangulation r1 r2 r3 r4 r5 r6)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0)   ptr r1
{-# LINE 125 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8)  ptr r2
{-# LINE 126 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16)  ptr r3
{-# LINE 127 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 128 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r5
{-# LINE 129 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40)     ptr r6
{-# LINE 130 "Triangulation.hsc" #-}

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
