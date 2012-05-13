package org.lbkgraph

class UndiEdgeAssoc[V](in: V) 
{
  def --- (out: V) = UndiEdge(in, out)
}