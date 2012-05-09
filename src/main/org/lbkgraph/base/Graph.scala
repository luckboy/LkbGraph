package org.lbkgraph.base
import org.lbkgraph._

trait Graph[V, E <: EdgeLike[V, E]] extends GraphLike[V, E, Graph[V, E]]
{
}