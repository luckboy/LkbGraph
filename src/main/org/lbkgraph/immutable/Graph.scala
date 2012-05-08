package org.lbkgraph.immutable
import org.lbkgraph._

trait Graph[+V, +E <: Product2[V, V]] extends org.lbkgraph.Graph[V, E] with GraphLike[V, E, Graph[V, E]]
{

}