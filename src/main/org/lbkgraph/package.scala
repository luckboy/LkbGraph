package org

/**
 * @author Łukasz Szpakowski
 */
package object lbkgraph 
{
  implicit def vertex[V](x: V) =
    Vertex(x)
    
  implicit def tuple2ToEdge[V](x: (V, V)) =
    Edge(x._1, x._2)
    
  implicit def tuple2ToWeightedEdge[V, W](x: ((V, W), V)) =
    WeightedEdge(x._1._1, x._2, x._1._2)    
}