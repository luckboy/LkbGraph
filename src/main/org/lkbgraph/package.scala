package org

/**
 * @author Łukasz Szpakowski
 */
package object lkbgraph 
{
  val V = Vertex
  
  implicit def tuple2ToDiEdge[V](x: (V, V)) =
    new UnwDiEdge(x._1, x._2)
  
  implicit def anyToUnwUndiEdgeAssoc[V](x: V) =
    new UnwUndiEdgeAssoc(x)
}