package org.lbkgraph.algorithm
import scala.math._
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A class for the strategy of the single source shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
trait ShortestPathStrategy 
{
  def shortestPathFromTo[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V, t: V)(implicit num: Numeric[W]): Option[Path[V, E]] =
    shortestPathAndMinDistFromTo[V, W, E, G](g, s, t).map { _._1 }

  def shortestPathsFrom[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V)(implicit num: Numeric[W]): Map[V, Path[V, E]] =
    shortestPathsAndMinDistsFrom[V, W, E, G](g, s).map { case (v, p) => (v, p._1) }

  def minDistanceFromTo[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V, t: V)(implicit num: Numeric[W]): Option[W] =
    shortestPathAndMinDistFromTo[V, W, E, G](g, s, t).map { _._2 }

  def minDistancesFrom[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V)(implicit num: Numeric[W]): Map[V, W] =
    shortestPathsAndMinDistsFrom[V, W, E, G](g, s).map { case (v, p) => (v, p._2) }

  def shortestPathAndMinDistFromTo[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V, t: V)(implicit num: Numeric[W]): Option[(Path[V, E], W)] =
    shortestPathsAndMinDistsFrom[V, W, E, G](g, s).find { case (v, s) => v == t }.map { _._2 }

  def shortestPathsAndMinDistsFrom[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V)(implicit num: Numeric[W]): Map[V, (Path[V, E], W)]
}