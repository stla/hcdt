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
    sizeOf    __ = 16
{-# LINE 22 "Triangulation.hsc" #-}
    alignment __ = 8
{-# LINE 23 "Triangulation.hsc" #-}
    peek ptr = do
      x'  <- (`peekByteOff` 0) ptr
{-# LINE 25 "Triangulation.hsc" #-}
      y'  <- (`peekByteOff` 8) ptr
{-# LINE 26 "Triangulation.hsc" #-}
      return CVertex { __x = x', __y = y' }
    poke ptr (CVertex r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 30 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 31 "Triangulation.hsc" #-}

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
    sizeOf    __ = 8
{-# LINE 45 "Triangulation.hsc" #-}
    alignment __ = 4
{-# LINE 46 "Triangulation.hsc" #-}
    peek ptr = do
      i'  <- (`peekByteOff` 0) ptr
{-# LINE 48 "Triangulation.hsc" #-}
      j'  <- (`peekByteOff` 4) ptr
{-# LINE 49 "Triangulation.hsc" #-}
      return CEdge { __i = i'
                   , __j = j' }
    poke ptr (CEdge r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 54 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r2
{-# LINE 55 "Triangulation.hsc" #-}

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
    sizeOf    __ = 12
{-# LINE 70 "Triangulation.hsc" #-}
    alignment __ = 4
{-# LINE 71 "Triangulation.hsc" #-}
    peek ptr = do
      i1'  <- (`peekByteOff` 0) ptr
{-# LINE 73 "Triangulation.hsc" #-}
      i2'  <- (`peekByteOff` 4) ptr
{-# LINE 74 "Triangulation.hsc" #-}
      i3'  <- (`peekByteOff` 8) ptr
{-# LINE 75 "Triangulation.hsc" #-}
      return CTriangle { __i1 = i1'
                       , __i2 = i2'
                       , __i3 = i3' }
    poke ptr (CTriangle r1 r2 r3)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 81 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r2
{-# LINE 82 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r3
{-# LINE 83 "Triangulation.hsc" #-}

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
{-# LINE 102 "Triangulation.hsc" #-}
    alignment __ = 8
{-# LINE 103 "Triangulation.hsc" #-}
    peek ptr = do
      vs  <- (`peekByteOff` 0) ptr
{-# LINE 105 "Triangulation.hsc" #-}
      nvs <- (`peekByteOff` 8) ptr
{-# LINE 106 "Triangulation.hsc" #-}
      ts  <- (`peekByteOff` 16) ptr
{-# LINE 107 "Triangulation.hsc" #-}
      nts <- (`peekByteOff` 24) ptr
{-# LINE 108 "Triangulation.hsc" #-}
      es  <- (`peekByteOff` 32) ptr
{-# LINE 109 "Triangulation.hsc" #-}
      nes <- (`peekByteOff` 40) ptr
{-# LINE 110 "Triangulation.hsc" #-}
      return CTriangulation { __vertices   = vs
                            , __nvertices  = nvs
                            , __triangles  = ts
                            , __ntriangles = nts
                            , __edges      = es
                            , __nedges     = nes }
    poke ptr (CTriangulation r1 r2 r3 r4 r5 r6)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0)   ptr r1
{-# LINE 119 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8)  ptr r2
{-# LINE 120 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16)  ptr r3
{-# LINE 121 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 122 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r5
{-# LINE 123 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40)     ptr r6
{-# LINE 124 "Triangulation.hsc" #-}

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
