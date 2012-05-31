package org.lkbgraph.base
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
trait AdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends AdjListGraphLike[V, X, E, AdjListGraph[V, X, E]] with Graph[V, X, E] 
{
  override def empty: AdjListGraph[V, X, E] =
    AdjListGraph.empty
    
  override protected[this] def newBuilder: Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    AdjListGraph.newBuilder
    
  override def stringPrefix: String =
    "AdjListGraph"
}

/** A singleton for the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
object AdjListGraph extends GraphFactory[AdjListGraph]
{
  implicit def canBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[AdjListGraph[_, _, E1], GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    graphCanBuildFrom
  
  override def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: AdjListGraph[V, X, E] = 
    immutable.AdjListGraph.empty

  override def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    AdjListGraph.newBuilder
}