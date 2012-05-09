package org.lbkgraph.base
import org.lbkgraph._

trait Tree[V, E <: EdgeLike[V, E]] extends TreeLike[V, E, Tree[V, E]]
{
}