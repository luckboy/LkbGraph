package org

/**
 * @author Łukasz Szpakowski
 */
package object lbkgraph 
{
  implicit def vertex[V](x: V) =
    Vertex(x)
    
  implicit def tuple2ToDiEdge[V](x: (V, V)) =
    DiEdge(x._1, x._2)
    
  implicit def tuple2ToWeightedDiEdge[V, W](x: ((V, W), V)) =
    WeightedDiEdge(x._1._1, x._2, x._1._2)    
    
  implicit def tuple2ToUndiEdge[V](x: (V, V)) =
    UndiEdge(x._1, x._2)

  implicit def tuple2ToWeightedUndiEdge[V, W](x: ((V, W), V)) =
    WeightedUndiEdge(x._1._1, x._2, x._1._2)    
}