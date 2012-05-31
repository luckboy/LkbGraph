package org.lkbgraph.immutable
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.GraphLike[V, X, E, Graph[V, X, E]] with base.Graph[V, X, E] with Set[GraphParam[V, X, E]]
{
  override def empty: Graph[V, X, E] =
    Graph.empty
  
  override protected[this] def newBuilder: Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    Graph.newBuilder
}

/** A singleton for immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
object Graph extends GraphFactory[Graph]
{
  implicit def canBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[Graph[_, _, E1], GraphParam[V, X, E], Graph[V, X, E]] =
    graphCanBuildFrom

  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Graph[V, X, E] = 
    AdjListGraph.empty
}