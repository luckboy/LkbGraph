package org.lkbgraph.mutable
import org.lkbgraph._

/** A trait for mutable graph. 
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.GraphLike[V, X, E, Graph[V, X, E]] with base.Graph[V, X, E] 
{
  override def empty: Graph[V, X, E] = throw new Exception
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], Graph[V, X, E]] = throw new Exception
}