package org.lkbgraph.algorithm
import scala.math.Numeric
import org.lkbgraph._
import org.lkbgraph.immutable._

/** A class for the all pairs shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
class AllPairsShortestPath[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G)(implicit num: Numeric[W])
{
  /** Finds the shortest paths for all pairs. If any path from the some two vertices is non-exist, 
   * there doesn't return the path for two vertices.
   */
  def allShortestPaths(implicit strategy: AllPairsShortestPathStrategy): Map[(V, V), (Path[V, Weighted[W], E])] =
    strategy.allShortestPaths[V, W, E, G](g)
    
  /** Finds the minimum distances for all pairs. If any path from the some two vertices is non-exist, 
   * there doesn't return the path for two vertices.
   */
  def allMinDistances(implicit strategy: AllPairsShortestPathStrategy): Map[(V, V), W] =
    strategy.allMinDistances[V, W, E, G](g)

  /** Finds the shortest path with the minimum distances for all pairs. If any path from the some two vertices is non-exist, 
   * there doesn't return the path for two vertices.
   */
  def allShortestPathAndMinDists(implicit strategy: AllPairsShortestPathStrategy): Map[(V, V), (Path[V, Weighted[W], E], W)] =
    strategy.allShortestPathsAndMinDists[V, W, E, G](g)
}

/** A singleton for the all pairs shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
object AllPairsShortestPath
{
  implicit def graphToAllPairsShortestPath[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: base.GraphBound[V, Weighted[W], E, G])(implicit num: Numeric[W]) =
    new AllPairsShortestPath[V, W, E, base.GraphBound[V, Weighted[W], E, G]](g)
}