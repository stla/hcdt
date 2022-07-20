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
  VertexT* vertices;
  unsigned nvertices;
  TriangleT* triangles;
  unsigned ntriangles;
  EdgeT* edges;
  unsigned nedges;
} TriangulationT;