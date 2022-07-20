# hcdt

### Delaunay triangulation 

```haskell
ghci> import Text.Show.Pretty
ghci> import Hcdt
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
ghci> import Hcdt
ghci> vertices = [Vertex 1 0, Vertex 3 0, Vertex 2 1, Vertex 0 0, Vertex 4 0, Vertex 2 2]
ghci> edges = [Edge 0 1, Edge 0 2, Edge 1 2]
ghci> triangulation <- cdelaunay vertices edges
ghci> pPrint triangulation

ghci> pPrint $ borderEdges triangulation
fromList [ Edge 1 0 , Edge 1 2 , Edge 0 3 , Edge 2 3 ]
```