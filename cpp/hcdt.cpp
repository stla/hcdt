#include <cstdlib> // to use malloc
#include "hcdt.hpp"
#include "CDT.h"

extern "C"
{

TriangulationT* delaunay(VertexT* points, size_t npoints){
  CDT::Triangulation<double> cdt(CDT::VertexInsertionOrder::AsProvided);
  // insert vertices
  std::vector<CDT::V2d<double>> vertices(npoints);
  for(size_t k = 0; k < npoints; ++k) {
    const VertexT v = points[k];
    vertices[k] = CDT::V2d<double>::make(v.x, v.y);
  }
  cdt.insertVertices(vertices);
  cdt.eraseSuperTriangle();
  //// output
  // triangles
  const CDT::TriangleVec triangles = cdt.triangles;
  const size_t ntriangles = triangles.size();
  TriangleT* out_triangles = (TriangleT*)malloc(ntriangles * sizeof(TriangleT));
  for(size_t k = 0; k < ntriangles; ++k){
    const CDT::VerticesArr3 trgl = triangles[k].vertices;
    out_triangles[k].i1 = trgl[0];
    out_triangles[k].i2 = trgl[1];
    out_triangles[k].i3 = trgl[2];
  }
  // all edges
  CDT::EdgeUSet allEdges = CDT::extractEdgesFromTriangles(triangles);
  const size_t nedges = allEdges.size();
  EdgeT* out_alledges = (EdgeT*)malloc(nedges * sizeof(EdgeT));
  std::unordered_set<CDT::Edge> :: iterator it;
  size_t k = 0;
  for(it = allEdges.begin(); it != allEdges.end(); it++){
    const CDT::Edge edge = *it;
    out_alledges[k].i = CDT::edge_get_v1(edge);
    out_alledges[k].j = CDT::edge_get_v2(edge);
    k++;
  }
  /* output mesh */
  TriangulationT* out = (TriangulationT*)malloc(sizeof(TriangulationT));
  out->vertices = points;
  out->nvertices = npoints;
  out->triangles = out_triangles;
  out->ntriangles = ntriangles;
  out->edges = out_alledges;
  out->nedges = nedges;
  return out;
}

}