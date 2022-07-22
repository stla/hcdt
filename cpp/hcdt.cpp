#include <cstdlib> // to use malloc
#include <stdio.h> // to use printf
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

CTriangulationT* cdelaunay(VertexT* points, size_t npoints, EdgeT* fedges, size_t nfedges){
  CDT::Triangulation<double> cdt(
    CDT::VertexInsertionOrder::AsProvided,
    CDT::IntersectingConstraintEdges::Resolve,
    0.0
  );
  // insert vertices
  std::vector<CDT::V2d<double>> vertices(npoints);
  for(size_t k = 0; k < npoints; ++k) {
    const VertexT v = points[k];
    vertices[k] = CDT::V2d<double>::make(v.x, v.y);
  }
  // insert edges
  std::vector<CDT::Edge> Edges;
  Edges.reserve(nfedges);
  for (size_t k = 0; k < nfedges; ++k) {
    const EdgeT e = fedges[k];
    Edges.push_back(CDT::Edge(e.i, e.j));
  }
  cdt.insertVertices(vertices);
  cdt.insertEdges(Edges);
  cdt.eraseOuterTrianglesAndHoles();
  //// output
  // vertices
  const std::vector<CDT::V2d<double>> cdt_vertices = cdt.vertices;
  const size_t nvertices = cdt_vertices.size();
  VertexT* out_vertices = (VertexT*)malloc(nvertices * sizeof(VertexT));
  for(size_t k = 0; k < nvertices; ++k){
    const CDT::V2d<double> v = cdt_vertices[k];
    out_vertices[k].x = v.x;
    out_vertices[k].y = v.y;
  }
  // triangles
  const CDT::TriangleVec triangles = cdt.triangles;
  const size_t ntriangles = triangles.size();
  //printf("ntriangles: %u\n", ntriangles);
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
  // fixed edges
  CDT::EdgeUSet fixedEdges = cdt.fixedEdges;
  const size_t nfixededges = fixedEdges.size();
  EdgeT* out_fixededges = (EdgeT*)malloc(nfixededges * sizeof(EdgeT));
  std::unordered_set<CDT::Edge> :: iterator itedge;
  size_t l = 0;
  for(itedge = fixedEdges.begin(); itedge != fixedEdges.end(); itedge++){
    const CDT::Edge edge = *itedge;
    out_fixededges[l].i = CDT::edge_get_v1(edge);
    out_fixededges[l].j = CDT::edge_get_v2(edge);
    l++;
  }
  /* output mesh */
  CTriangulationT* out = (CTriangulationT*)malloc(sizeof(CTriangulationT));
  out->vertices = out_vertices;
  out->nvertices = nvertices;
  out->triangles = out_triangles;
  out->ntriangles = ntriangles;
  out->edges = out_alledges;
  out->nedges = nedges;
  out->fixededges = out_fixededges;
  out->nfixededges = nfixededges;
  return out;
}

}