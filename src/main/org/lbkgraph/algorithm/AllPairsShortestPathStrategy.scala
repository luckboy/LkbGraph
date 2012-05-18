package org.lbkgraph.algorithm
import scala.math.Numeric
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A class for the strategy of the all pairs shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
trait AllPairsShortestPathStrategy 
{
  def allShortestPaths[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G)(implicit num: Numeric[W]): Map[(V, V), Path[V, E]] =
    allShortestPathsAndMinDists[V, W, E, G](g).map { case (p1, p2) => (p1, p2._1) }

  def allMinDistances[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G)(implicit num: Numeric[W]): Map[(V, V), W] =
    allShortestPathsAndMinDists[V, W, E, G](g).map { case (p1, p2) => (p1, p2._2) }

  def allShortestPathsAndMinDists[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G)(implicit num: Numeric[W]): Map[(V, V), (Path[V, E], W)]
}