package org.lkbgraph.base
import org.lkbgraph._

/** A template trait for the implementation of the incidence set representation.
 * 
 * @author Łukasz Szpaklowski
 */
trait IncSetGraphLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: IncSetGraphLike[V, X, E, G] with Graph[V, X, E]] extends GraphLike[V, X, E, G]
{
  protected def privVertexSet: collection.Set[V]
  
  protected def edgeMap: collection.Map[E[V, Unweighted], E[V, X]]

  protected def newIncSetGraph(vs: collection.Set[V], es: collection.Map[E[V, Unweighted], E[V, X]]): G
  
  override def vertices: Iterable[V] =
    privVertexSet
    
  override def edges: Iterable[E[V, X]] =
    edgeMap.values
    
  override def +@ (v: V): G =
    newIncSetGraph(vertexSet + v, edgeMap.toMap)
    
  override def +~ (e: E[V, X]): G =
    if(e._1 != e._2) newIncSetGraph((vertexSet + e._1) + e._2 , edgeMap + (e.toUnweightedEdge -> e)) else repr
  
  override def -@ (v: V): G =
    newIncSetGraph(vertexSet - v, edgeMap.filterNot { case (e, _) => e.in == v && (e.isDirected || e.out == v) })
  
  override def -~! (e: E[V, Unweighted]): G =
    if(e._1 != e._2) newIncSetGraph(vertexSet, edgeMap - e) else repr
}