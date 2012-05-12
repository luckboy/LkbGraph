package org.lbkgraph.immutable
import org.lbkgraph._

trait Forest[V, E <: EdgeLike[V, E]] extends ForestLike[V, E, Graph[V, E], Forest[V, E]] with Graph[V, E]
{
  override def empty: Graph[V, E] = throw new Exception
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, E], Graph[V, E]] = throw new Exception
}