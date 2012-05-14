package org.lbkgraph

class UndiEdgeAssoc[V](x: V) 
{
  def ~ (y: V) = UndiEdge(x, y)
}