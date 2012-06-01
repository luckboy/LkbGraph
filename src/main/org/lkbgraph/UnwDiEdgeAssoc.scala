package org.lkbgraph

class UnwDiEdgeAssoc[V](v: V) 
{
  def ~> (u: V): UnwDiEdge[V] =
    UnwDiEdge[V](v, u)
}