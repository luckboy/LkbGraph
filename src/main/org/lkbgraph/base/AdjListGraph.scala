package org.lkbgraph.base
import org.lkbgraph._

/** A trait for the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
trait AdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends AdjListGraphLike[V, X, E, AdjListGraph[V, X, E]] with Graph[V, X, E] 
{
  override def empty: AdjListGraph[V, X, E] =
    AdjListGraph.empty
    
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    AdjListGraph.newBuilder
}

/** A singleton for the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
object AdjListGraph
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: AdjListGraph[V, X, E] = 
    immutable.AdjListGraph.empty
    
  def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: collection.mutable.Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    immutable.AdjListGraph.newBuilder
}