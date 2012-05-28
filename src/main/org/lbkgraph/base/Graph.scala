package org.lbkgraph.base
import org.lbkgraph._

/** A trait for the immutable graph and the mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends GraphLike[V, X, E, Graph[V, X, E]] with Set[GraphParam[V, X, E]]
{
  override def empty: Graph[V, X, E] = throw new Exception
    immutable.Graph.empty
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], Graph[V, X, E]] = throw new Exception
    immutable.Graph.newBuilder
}