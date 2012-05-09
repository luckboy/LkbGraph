package org.lbkgraph.immutable
import org.lbkgraph._

trait Tree[V, E <: EdgeLike[V, E]] extends base.TreeLike[V, E, Tree[V, E]] with base.Tree[V, E]
{
  
}
