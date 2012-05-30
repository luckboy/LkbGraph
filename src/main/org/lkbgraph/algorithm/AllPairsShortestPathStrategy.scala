package org.lkbgraph.algorithm
import scala.math.Numeric
import org.lkbgraph._
import org.lkbgraph.immutable._

/** A class for the strategy of the all pairs shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
trait AllPairsShortestPathStrategy 
{
  def allShortestPaths[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G)(implicit num: Numeric[W]): Map[(V, V), Path[V, Weighted[W], E]] =
    allShortestPathsAndMinDists[V, W, E, G](g).map { case (p1, p2) => (p1, p2._1) }

  def allMinDistances[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G)(implicit num: Numeric[W]): Map[(V, V), W] =
    allShortestPathsAndMinDists[V, W, E, G](g).map { case (p1, p2) => (p1, p2._2) }

  def allShortestPathsAndMinDists[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G)(implicit num: Numeric[W]): Map[(V, V), (Path[V, Weighted[W], E], W)]
}