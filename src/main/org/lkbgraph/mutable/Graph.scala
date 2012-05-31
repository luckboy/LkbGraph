package org.lkbgraph.mutable
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for mutable graph. 
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends GraphLike[V, X, E, Graph[V, X, E]] with base.Graph[V, X, E] with collection.mutable.Set[GraphParam[V, X, E]]
{
  override def empty: Graph[V, X, E] =
    Graph.empty
  
  override protected[this] def newBuilder: Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    newBuilder
}

/** A singleton for mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
object Graph
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Graph[V, X, E] = 
    AdjListGraph.empty  
}