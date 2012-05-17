package org.lbkgraph.algorithm
import scala.math.Numeric
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A class for the all pairs shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
class AllPairsShortestPath[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G)(implicit num: Numeric[W])
{
  /** Finds the shortest paths for all pairs. If any path from the some two vertices is non-exist, 
   * there doesn't return the path for two vertices.
   * @return			the shortest paths with the distances.
   */
  def allShortestPaths(implicit strategy: AllPairsShortestPathStrategy): Map[(V, V), (W, Path[V, E])] =
    strategy.allShortestPaths[V, W, E, G](g)
}

/** A singleton for the all pairs shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
object AllPairsShortestPath
{
  implicit def graphToAllPairsShortestPath[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: base.GraphBound[V, E, G])(implicit num: Numeric[W]) =
    new AllPairsShortestPath[V, W, E, base.GraphBound[V, E, G]](g)
}