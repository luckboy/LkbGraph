package org

/**
 * @author Łukasz Szpakowski
 */
package object lkbgraph 
{
  implicit def tuple2ToDiEdge[V](x: (V, V)) =
    new UnwDiEdge(x._1, x._2)
  
  implicit def anyToUndiEdgeAssoc[V](x: V) =
    new UndiEdgeAssoc(x)
}