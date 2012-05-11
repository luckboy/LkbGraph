package org.lbkgraph.base
import org.lbkgraph._

/** A trait for the immutable tree and the mutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait Tree[V, E <: EdgeLike[V, E]] extends TreeLike[V, E, Graph[V, E], Tree[V, E]] with Graph[V, E]
{
  override def empty: Graph[V, E] = throw new Exception
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, E], Graph[V, E]] = throw new Exception
}