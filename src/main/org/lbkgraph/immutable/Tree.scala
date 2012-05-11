package org.lbkgraph.immutable
import org.lbkgraph._

/** A trait for the immutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait Tree[V, E <: EdgeLike[V, E]] extends base.TreeLike[V, E,  Graph[V, E], Tree[V, E]] with base.Tree[V, E]
{
  override def empty: Graph[V, E] = throw new Exception
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, E], Graph[V, E]] = throw new Exception  
}
