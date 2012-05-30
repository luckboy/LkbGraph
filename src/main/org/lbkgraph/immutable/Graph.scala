package org.lbkgraph.immutable
import scala.collection.immutable.ListMap
import org.lbkgraph._

/** A trait for the immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.GraphLike[V, X, E, Graph[V, X, E]] with base.Graph[V, X, E]
{
  override def empty: Graph[V, X, E] =
    Graph.empty
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    Graph.newBuilder
}

/** A singleton for the immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
object Graph
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Graph[V, X, E] = 
    AdjListGraph.empty
  
  def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: collection.mutable.Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    AdjListGraph.newBuilder
  
  /** Creates a new graph from the vertices and / or the edges.
   * @param params		the vertices and / or the edges.
   * @return			a new graph.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](params: GraphParam[V, X, E]*): Graph[V, X, E] =
    (newBuilder ++= params).result
}