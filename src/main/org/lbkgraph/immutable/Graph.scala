package org.lbkgraph.immutable
import org.lbkgraph._

trait Graph[V, E <: EdgeLike[V, E]] extends base.GraphLike[V, E, Graph[V, E]] with base.Graph[V, E]
{

}