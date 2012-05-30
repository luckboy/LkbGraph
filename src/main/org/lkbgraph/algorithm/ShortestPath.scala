package org.lkbgraph.algorithm
import scala.math.Numeric
import org.lkbgraph._
import org.lkbgraph.immutable._

/** A class for the single source shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
class ShortestPath[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: G)(implicit num: Numeric[W])
{  
  /** Finds the shortest path from the start vertex to the end vertex.
   * @param s			the start vertex.
   * @param t			the end vertex.
   * @return			the shortest path if two specified vertices are indirectly connected or directly connected, None 
   * 					otherwise.
   */
  def shortestPathFromTo(s: V, t: V)(implicit strategy: ShortestPathStrategy): Option[Path[V, Weighted[W], E]] =
    strategy.shortestPathFromTo[V, W, E, G](g, s, t)
  
  /** Finds the shortest paths from the specified vertex. If any path from the start vertex to some vertex is non-exist, 
   * there doesn't return the path for the pair.
   * @param s			the start vertex.
   * @return			the shortest paths.
   */
  def shortestPathsFrom(s: V)(implicit strategy: ShortestPathStrategy): Map[V, Path[V, Weighted[W], E]] =
    strategy.shortestPathsFrom[V, W, E, G](g, s)
  
  /** Finds the minimum distance from the start vertex to the end vertex.
   * @param s			the start vertex.
   * @param t			the end vertex.
   * @return			the minimum distance if this two vertices are indirectly connected or directly connected, None 
   * 					otherwise.
   */
  def minDistanceFromTo(s: V, t: V)(implicit strategy: ShortestPathStrategy): Option[W] =
    strategy.minDistanceFromTo[V, W, E, G](g, s, t)
  
  /** Finds the minimum distances from the start vertex. If any path from the start vertex to some vertex is non-exist, 
   * there doesn't return the path for the vertex pair.
   * @param s			the start vertex.
   * @return			the minimum distances.
   */
  def minDistancesFrom(s: V)(implicit strategy: ShortestPathStrategy): Map[V, W] =
    strategy.minDistancesFrom[V, W, E, G](g, s)    
  
  /** Finds the shortest path with the minimum distance from the start vertex to the end vertex.
   * @param s			the start vertex.
   * @param t			the end vertex.
   * @return			the shorted path and the minimum distance if this two vertices are indirectly connected, None 
   * 					otherwise.
   */
  def shortestPathAndMinDistFromTo(s: V, t: V)(implicit strategy: ShortestPathStrategy): Option[(Path[V, Weighted[W], E], W)] =
    strategy.shortestPathAndMinDistFromTo[V, W, E, G](g, s, t)
  
  /** Finds the shortest paths with the minimum distances from the start vertex. If any path from the start vertex to some vertex is non-exist, 
   * there doesn't return the path for the pair.
   * @param s			the start vertex.
   * @return			the the shortest paths with the minimum distances.
   */
  def shortestPathAndMinDistsFrom(s: V, t: V)(implicit strategy: ShortestPathStrategy): Map[V, (Path[V, Weighted[W], E], W)] =
    strategy.shortestPathsAndMinDistsFrom[V, W, E, G](g, s)
}

/** A singleton for the single source shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
object ShortestPath
{
  implicit def graphToShortestPath[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: base.GraphBound[V, Weighted[W], E, G])(implicit num: Numeric[W]) =
    new ShortestPath[V, W, E, base.GraphBound[V, Weighted[W], E, G]](g)
}