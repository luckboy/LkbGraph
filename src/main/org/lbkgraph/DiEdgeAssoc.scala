package org.lbkgraph

class DiEdgeAssoc[V](in: V)
{
  def --> (out: V) = DiEdge(in, out)
}