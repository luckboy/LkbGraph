package org.lkbgraph

class UndiEdgeAssoc[V](x: V) 
{
  def ~ (y: V) = UnwUndiEdge(x, y)
}