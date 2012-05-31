package org.lkbgraph.base
import org.lkbgraph._

/** A trait for immutable graph and / or  mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends GraphLike[V, X, E, Graph[V, X, E]] with collection.Set[GraphParam[V, X, E]]
{
  override def empty: Graph[V, X, E] =
    immutable.Graph.empty
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    immutable.Graph.newBuilder
}