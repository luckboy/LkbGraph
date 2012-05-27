package org

/**
 * @author Łukasz Szpakowski
 */
package object lbkgraph 
{
  implicit def tuple2ToDiEdge[V](x: (V, V)) =
    new UnwDiEdge(x._1, x._2)
  
  implicit def anyToUndiEdgeAssoc[V](x: V) =
    new UndiEdgeAssoc(x)

  implicit def edgeSetCanFindEdge[V, E <: EdgeLike[V, E]] =
    new CanFind[Iterable[E], E, E] {
      override def find(es: Iterable[E])(e1: E): Option[E] =
        es.find(e1 ==)
    }
  
  implicit def wEdgeSetCanFindUnwEdge[V, E <: WEdgeLike[V, _, E1, E], E1 <: UnwEdgeLike[V, E1]] =
    new CanFind[Iterable[E], E, E1] {
      override def find(es: Iterable[E])(e1: E1): Option[E] =
        es.find { e1 == _.toUnweightedEdge }
    }
  
  implicit def undiEdgeSetCanFindDiEdge[V, E <: UndiEdgeLike[V, _, E], E1 <: DiEdgeLike[V, E2, E1], E2 <: GenUndiEdge[V]](implicit cf: CanFind[Iterable[E], E, E2]) =
    new CanFind[Iterable[E], E, E1] {
      override def find(es: Iterable[E])(e1: E1): Option[E] =
        cf.find(es)(e1.toUndirectedEdge)
    }
  
  implicit def diEdgeSetCanFindUndiEdge[V, E <: DiEdgeLike[V, E2, E], E1 <: UndiEdgeLike[V, E3, E1], E2 <: GenUndiEdge[V], E3](implicit cf: CanFind[Iterable[E], E, E3]) =
    new CanFind[Iterable[E], E2, E1] {
      override def find(es: Iterable[E])(e1: E1): Option[E2] =
        cf.find(es)(e1.directedEdges._1).flatMap { _ => cf.find(es)(e1.directedEdges._2).map { _.toUndirectedEdge } }
    }
}