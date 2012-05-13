package org

/**
 * @author Łukasz Szpakowski
 */
package object lbkgraph 
{
  implicit def vertex[V](x: V) =
    Vertex(x)
    
  implicit def anyToDiEdgeAssoc[V](x: V) =
    new DiEdgeAssoc(x)

  implicit def anyToUndiEdgeAssoc[V](x: V) =
    new UndiEdgeAssoc(x)

  implicit def anyToWeightedEdgeAssoc[V](x: V) =
    new WeightedEdgeAssoc(x)
}