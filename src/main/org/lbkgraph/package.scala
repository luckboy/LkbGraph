package org

/**
 * @author Łukasz Szpakowski
 */
package object lbkgraph 
{
  import generic.CanFind
  import generic.FindStrategy
  
  implicit def vertex[V](x: V) =
    Vertex(x)
    
  implicit def anyToDiEdgeAssoc[V](x: V) =
    new DiEdgeAssoc(x)

  implicit def anyToUndiEdgeAssoc[V](x: V) =
    new UndiEdgeAssoc(x)

  implicit def anyToWeightedEdgeAssoc[V](x: V) =
    new WeightedEdgeAssoc(x)
  
  implicit def edgeSetCanFindEdge[V, E <: EdgeLike[V, E]] =
    new CanFind[Set[E], E, E] {
      override def apply(): FindStrategy[Set[E], E, E] =
        new FindStrategy[Set[E], E, E] {
          override def find(es: Set[E])(e1: E): Option[E] =
            es.find(e1 ==)
        }
    }
  
  implicit def weightedEdgeSetCanFindUnweightedEdge[V, E <: WeightedEdgeLike[V, _, E1, E], E1 <: UnweightedEdgeLike[V, E1]] =
    new CanFind[Set[E], E, E1] {
      override def apply(): FindStrategy[Set[E], E, E1] =
        new FindStrategy[Set[E], E, E1] {
          override def find(es: Set[E])(e1: E1): Option[E] =
            es.find { e1 == _.unweightedEdge }
        }
    }
}