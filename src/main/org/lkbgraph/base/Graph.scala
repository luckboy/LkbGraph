package org.lkbgraph.base
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for immutable graph and / or  mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends GraphLike[V, X, E, Graph[V, X, E]] with collection.Set[GraphParam[V, X, E]]
{
  override def empty: Graph[V, X, E] =
    immutable.Graph.empty
  
  override protected[this] def newBuilder: Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    immutable.Graph.newBuilder
}

/** A trait for immutable graph and / or  mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
object Graph extends GraphFactory[Graph]
{
  implicit def canBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[Graph[_, _, E1], GraphParam[V, X, E], Graph[V, X, E]] =
    graphCanBuildFrom

  override def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Graph[V, X, E] = 
    AdjListGraph.empty

  override def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    AdjListGraph.newBuilder
}