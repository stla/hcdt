# hcdt

<!-- badges: start -->
[![Stack-lts](https://github.com/stla/hcdt/actions/workflows/Stack-lts.yml/badge.svg)](https://github.com/stla/hcdt/actions/workflows/Stack-lts.yml)
[![Stack-nightly](https://github.com/stla/hcdt/actions/workflows/Stack-nightly.yml/badge.svg)](https://github.com/stla/hcdt/actions/workflows/Stack-nightly.yml)
<!-- badges: end -->


### Delaunay triangulation 

```haskell
ghci> import Text.Show.Pretty
ghci> import Geometry.HCDT
ghci> vertices = [Vertex 0 0, Vertex 0 1, Vertex 1 1, Vertex 1 0]
ghci> triangulation <- delaunay vertices
ghci> pPrint triangulation
Triangulation
  { _vertices =
      fromList
        [ ( 0 , Vertex 0.0 0.0 )
        , ( 1 , Vertex 0.0 1.0 )
        , ( 2 , Vertex 1.0 1.0 )
        , ( 3 , Vertex 1.0 0.0 )
        ]
  , _triangles = [ Triangle 1 0 2 , Triangle 2 0 3 ]
  , _edges = [ Edge 2 3 , Edge 0 3 , Edge 1 2 , Edge 0 2 , Edge 0 1 ]
  }
ghci> pPrint $ borderEdges triangulation
fromList [ Edge 1 0 , Edge 1 2 , Edge 0 3 , Edge 2 3 ]
```


### Constrained Delaunay triangulation 

```haskell
ghci> import Text.Show.Pretty
ghci> import Geometry.HCDT
ghci> vertices = [Vertex 1 1, Vertex 3 1, Vertex 2 2, Vertex 0 0, Vertex 4 0, Vertex 2 5]
ghci> edges = [Edge 0 1, Edge 0 2, Edge 1 2, Edge 3 4, Edge 3 5, Edge 4 5]
ghci> triangulation <- cdelaunay vertices edges
ntriangles: 6
ghci> pPrint triangulation
ConstrainedTriangulation
  { _triangulation =
      Triangulation
        { _vertices =
            fromList
              [ ( 0 , Vertex 1.0 1.0 )
              , ( 1 , Vertex 3.0 1.0 )
              , ( 2 , Vertex 2.0 2.0 )
              , ( 3 , Vertex 0.0 0.0 )
              , ( 4 , Vertex 4.0 0.0 )
              , ( 5 , Vertex 2.0 5.0 )
              ]
        , _triangles =
            [ Triangle 1 0 3
            , Triangle 2 1 5
            , Triangle 2 5 0
            , Triangle 0 5 3
            , Triangle 1 4 5
            , Triangle 1 3 4
            ]
        , _edges =
            [ Edge 3 4
            , Edge 1 4
            , Edge 3 5
            , Edge 0 5
            , Edge 2 5
            , Edge 0 2
            , Edge 1 5
            , Edge 1 2
            , Edge 1 3
            , Edge 4 5
            , Edge 0 3
            , Edge 0 1
            ]
        }
  , _fixedEdges =
      [ Edge 0 1 , Edge 0 2 , Edge 4 5 , Edge 1 2 , Edge 3 4 , Edge 3 5 ]
  }
```