package org.lkbgraph

class UnwUndiEdgeAssoc[V](x: V) 
{
  def ~ (y: V) = UnwUndiEdge(x, y)
}