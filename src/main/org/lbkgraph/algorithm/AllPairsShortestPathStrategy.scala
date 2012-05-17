package org.lbkgraph.algorithm
import scala.math.Numeric
import org.lbkgraph._
import org.lbkgraph.immutable._

trait AllPairsShortestPathStrategy 
{
  def allShortestPaths[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G)(implicit num: Numeric[W]): Map[(V, V), (W, Path[V, E])]
}