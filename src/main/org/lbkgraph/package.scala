package org

/**
 * @author Łukasz Szpakowski
 */
package object lbkgraph 
{
  implicit def tuple2ToDiEdge[V](x: (V, V)) =
    new DiEdge(x._1, x._2)
  
  implicit def anyToUndiEdgeAssoc[V](x: V) =
    new UndiEdgeAssoc(x)

  implicit def edgeSetCanFindEdge[V, E <: EdgeLike[V, E]] =
    new CanFind[Set[E], E, E] {
      override def find(es: Set[E])(e1: E): Option[E] =
        es.find(e1 ==)
    }
  
  implicit def weightedEdgeSetCanFindUnweightedEdge[V, E <: WeightedEdgeLike[V, _, E1, E], E1 <: UnweightedEdgeLike[V, E1]] =
    new CanFind[Set[E], E, E1] {
      override def find(es: Set[E])(e1: E1): Option[E] =
        es.find { e1 == _.toUnweightedEdge }
    }
  
  implicit def undiEdgeSetCanFindDiEdge[V, E <: UndiEdgeLike[V, _, E], E1 <: DiEdgeLike[V, E2, E1], E2](implicit cf: CanFind[Set[E], E, E2]) =
    new CanFind[Set[E], E, E1] {
      override def find(es: Set[E])(e1: E1): Option[E] =
        cf.find(es)(e1.toUndirectedEdge)
    }
  
  implicit def diEdgeSetCanFindUndiEdge[V, E <: DiEdgeLike[V, E2, E], E1 <: UndiEdgeLike[V, E3, E1], E2, E3](implicit cf: CanFind[Set[E], E, E3]) =
    new CanFind[Set[E], E2, E1] {
      override def find(es: Set[E])(e1: E1): Option[E2] =
        cf.find(es)(e1.directedEdges._1).flatMap { _ => cf.find(es)(e1.directedEdges._2).map { _.toUndirectedEdge } }
    }
}