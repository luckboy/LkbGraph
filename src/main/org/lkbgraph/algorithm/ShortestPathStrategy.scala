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
  def shortestPathFromTo[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G, s: V, t: V)(implicit num: Numeric[W]): Option[Path[V, Weighted[W], E]] =
    shortestPathAndMinDistFromTo[V, W, E, G](g, s, t).map { _._1 }

  def shortestPathsFrom[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G, s: V)(implicit num: Numeric[W]): Map[V, Path[V, Weighted[W], E]] =
    shortestPathsAndMinDistsFrom[V, W, E, G](g, s).map { case (v, p) => (v, p._1) }

  def minDistanceFromTo[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G, s: V, t: V)(implicit num: Numeric[W]): Option[W] =
    shortestPathAndMinDistFromTo[V, W, E, G](g, s, t).map { _._2 }

  def minDistancesFrom[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G, s: V)(implicit num: Numeric[W]): Map[V, W] =
    shortestPathsAndMinDistsFrom[V, W, E, G](g, s).map { case (v, p) => (v, p._2) }

  def shortestPathAndMinDistFromTo[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G, s: V, t: V)(implicit num: Numeric[W]): Option[(Path[V, Weighted[W], E], W)] =
    shortestPathsAndMinDistsFrom[V, W, E, G](g, s).find { case (v, s) => v == t }.map { _._2 }

  def shortestPathsAndMinDistsFrom[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G, s: V)(implicit num: Numeric[W]): Map[V, (Path[V, Weighted[W], E], W)]
}