package org.lbkgraph

class WeightedEdgeAssoc[V](in: V)
{
  class Assoc[W](weight: W)
  {
    def -> (out: V) = WeightedDiEdge(in, out, weight)
    
    def -- (out: V) = WeightedUndiEdge(in, out, weight)
  }
  
  def -~ [W](weight: W) = new Assoc(weight)
}