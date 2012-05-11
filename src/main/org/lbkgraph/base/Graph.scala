package org.lbkgraph.base
import org.lbkgraph._

/** A trait for the immutable graph and the mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, E <: EdgeLike[V, E]] extends GraphLike[V, E, Graph[V, E]] with Set[GraphParam[V, E]]
{
  override def empty: Graph[V, E] = throw new Exception
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, E], Graph[V, E]] = throw new Exception
}