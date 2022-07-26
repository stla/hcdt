{-# LINE 1 "Triangulation.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Triangulation
( cTriangulationToTriangulation
, cCTriangulationToConstrainedTriangulation
, vertexToCVertex
, edgeToCEdge
, c_delaunay 
, c_cdelaunay
, CTriangulation (..)
, CVertex (..)
, CCTriangulation (..)
, CEdge (..) )
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
{-# LINE 29 "Triangulation.hsc" #-}
    alignment __ = 8
{-# LINE 30 "Triangulation.hsc" #-}
    peek ptr = do
      x'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 32 "Triangulation.hsc" #-}
      y'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 33 "Triangulation.hsc" #-}
      return CVertex { __x = x', __y = y' }
    poke ptr (CVertex r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 37 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 38 "Triangulation.hsc" #-}

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
{-# LINE 56 "Triangulation.hsc" #-}
    alignment __ = 4
{-# LINE 57 "Triangulation.hsc" #-}
    peek ptr = do
      i'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 59 "Triangulation.hsc" #-}
      j'  <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 60 "Triangulation.hsc" #-}
      return CEdge { __i = i'
                   , __j = j' }
    poke ptr (CEdge r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 65 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r2
{-# LINE 66 "Triangulation.hsc" #-}

cEdgeToEdge :: CEdge -> IO Edge
cEdgeToEdge cedge = do
  let i = fromIntegral $ __i cedge
  let j = fromIntegral $ __j cedge
  return $ Edge i j

edgeToCEdge :: Edge -> IO CEdge
edgeToCEdge (Edge i j) = do
  return $ CEdge { __i = fromIntegral i, __j = fromIntegral j }

data CTriangle = CTriangle {
    __i1 :: CUInt
  , __i2 :: CUInt
  , __i3 :: CUInt
}

instance Storable CTriangle where
    sizeOf    __ = (12)
{-# LINE 85 "Triangulation.hsc" #-}
    alignment __ = 4
{-# LINE 86 "Triangulation.hsc" #-}
    peek ptr = do
      i1'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 88 "Triangulation.hsc" #-}
      i2'  <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 89 "Triangulation.hsc" #-}
      i3'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 90 "Triangulation.hsc" #-}
      return CTriangle { __i1 = i1'
                       , __i2 = i2'
                       , __i3 = i3' }
    poke ptr (CTriangle r1 r2 r3)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 96 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r2
{-# LINE 97 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r3
{-# LINE 98 "Triangulation.hsc" #-}

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
{-# LINE 117 "Triangulation.hsc" #-}
    alignment __ = 8
{-# LINE 118 "Triangulation.hsc" #-}
    peek ptr = do
      vs  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 120 "Triangulation.hsc" #-}
      nvs <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 121 "Triangulation.hsc" #-}
      ts  <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 122 "Triangulation.hsc" #-}
      nts <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 123 "Triangulation.hsc" #-}
      es  <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 124 "Triangulation.hsc" #-}
      nes <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 125 "Triangulation.hsc" #-}
      return CTriangulation { __vertices   = vs
                            , __nvertices  = nvs
                            , __triangles  = ts
                            , __ntriangles = nts
                            , __edges      = es
                            , __nedges     = nes }
    poke ptr (CTriangulation r1 r2 r3 r4 r5 r6)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0)   ptr r1
{-# LINE 134 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8)  ptr r2
{-# LINE 135 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16)  ptr r3
{-# LINE 136 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 137 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r5
{-# LINE 138 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40)     ptr r6
{-# LINE 139 "Triangulation.hsc" #-}

data CCTriangulation = CCTriangulation {
    __vertices'    :: Ptr CVertex
  , __nvertices'   :: CSize
  , __triangles'   :: Ptr CTriangle
  , __ntriangles'  :: CSize
  , __edges'       :: Ptr CEdge
  , __nedges'      :: CSize
  , __fixededges'  :: Ptr CEdge
  , __nfixededges' :: CSize
}

instance Storable CCTriangulation where
    sizeOf    __ = (64)
{-# LINE 153 "Triangulation.hsc" #-}
    alignment __ = 8
{-# LINE 154 "Triangulation.hsc" #-}
    peek ptr = do
      vs   <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 156 "Triangulation.hsc" #-}
      nvs  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 157 "Triangulation.hsc" #-}
      ts   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 158 "Triangulation.hsc" #-}
      nts  <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 159 "Triangulation.hsc" #-}
      es   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 160 "Triangulation.hsc" #-}
      nes  <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 161 "Triangulation.hsc" #-}
      fes  <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 162 "Triangulation.hsc" #-}
      nfes <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 163 "Triangulation.hsc" #-}
      return CCTriangulation { __vertices'    = vs
                            , __nvertices'   = nvs
                            , __triangles'   = ts
                            , __ntriangles'  = nts
                            , __edges'       = es
                            , __nedges'      = nes 
                            , __fixededges'  = fes
                            , __nfixededges' = nfes }
    poke ptr (CCTriangulation r1 r2 r3 r4 r5 r6 r7 r8)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0)    ptr r1
{-# LINE 174 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8)   ptr r2
{-# LINE 175 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 176 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24)  ptr r4
{-# LINE 177 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32)       ptr r5
{-# LINE 178 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 179 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 48)  ptr r7
{-# LINE 180 "Triangulation.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r8
{-# LINE 181 "Triangulation.hsc" #-}

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

cCTriangulationToConstrainedTriangulation :: CCTriangulation -> IO ConstrainedTriangulation
cCTriangulationToConstrainedTriangulation cctriangulation = do
  let nvertices  = fromIntegral $ __nvertices' cctriangulation
      ntriangles = fromIntegral $ __ntriangles' cctriangulation
      nedges     = fromIntegral $ __nedges' cctriangulation
      nfedges    = fromIntegral $ __nfixededges' cctriangulation
  vertices  <- peekArray nvertices  (__vertices' cctriangulation)
  triangles <- peekArray ntriangles (__triangles' cctriangulation)
  edges     <- peekArray nedges     (__edges' cctriangulation)
  fedges    <- peekArray nfedges    (__fixededges' cctriangulation)
  vertices'  <- mapM cVertexToVertex vertices
  triangles' <- mapM cTriangleToTriangle triangles
  edges'     <- mapM cEdgeToEdge edges
  fedges'    <- mapM cEdgeToEdge fedges
  let triangulation = Triangulation { 
                          _vertices  = IM.fromAscList (zip [0 .. nvertices-1] vertices')
                        , _triangles = triangles'
                        , _edges     = edges' 
                      }
  return ConstrainedTriangulation { 
                          _triangulation = triangulation
                        , _fixedEdges    = fedges' }

foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CVertex -> CSize -> IO (Ptr CTriangulation)

foreign import ccall unsafe "cdelaunay" c_cdelaunay
  :: Ptr CVertex -> CSize -> Ptr CEdge -> CSize -> IO (Ptr CCTriangulation)
