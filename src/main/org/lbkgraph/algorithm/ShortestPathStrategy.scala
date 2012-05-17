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
  def shortestPathFromTo[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V, t: V)(implicit num: Numeric[W]): Option[(W, Path[V, E])]

  def shortestPathsFrom[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G, s: V)(implicit num: Numeric[W]): Map[V, (W, Path[V, E])]
}