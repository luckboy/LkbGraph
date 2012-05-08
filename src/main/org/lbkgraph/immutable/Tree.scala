package org.lbkgraph.immutable
import org.lbkgraph._

trait Tree[+V, +E <: Product2[V, V]] extends org.lbkgraph.Tree[V, E] with TreeLike[V, E, Graph[V, E]]
{
  
}
