typedef struct Vertex {
  double x;
  double y;
} VertexT;

typedef struct Edge {
  unsigned i;
  unsigned j;
} EdgeT;

typedef struct Triangle {
  unsigned i1;
  unsigned i2;
  unsigned i3;
} TriangleT;

typedef struct Triangulation {
  VertexT*   vertices;
  size_t     nvertices;
  TriangleT* triangles;
  size_t     ntriangles;
  EdgeT*     edges;
  size_t     nedges;
} TriangulationT;

typedef struct CTriangulation {
  VertexT*   vertices;
  size_t     nvertices;
  TriangleT* triangles;
  size_t     ntriangles;
  EdgeT*     edges;
  size_t     nedges;
  EdgeT*     fixededges;
  size_t     nfixededges;
} CTriangulationT;