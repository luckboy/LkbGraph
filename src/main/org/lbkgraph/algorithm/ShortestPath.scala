package org.lbkgraph.algorithm
import scala.math.Numeric
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A class for the single source shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
class ShortestPath[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: G)(implicit num: Numeric[W])
{
  /** Finds the shortest paths from the start vertex to the end vertex.
   * @param s			the start vertex.
   * @param t			the end vertex.
   * @return			the shortest paths with the distances if it is exists, None otherwise.
   */
  def shortestPathFromTo(s: V, t: V)(implicit strategy: ShortestPathStrategy): Option[(W, Path[V, E])] =
    strategy.shortestPathFromTo[V, W, E, G](g, s, t)
  
  /** Finds the shortest paths from the specified vertex. If any path from the start vertex to some vertex is non-exist, 
   * there doesn't return the path for start vertex and this vertex.
   * @param s			the start vertex.
   * @return			the shortest paths with the distances.
   */
  def shortestPathsFrom(s: V)(implicit strategy: ShortestPathStrategy): Map[V, (W, Path[V, E])] =
    strategy.shortestPathsFrom[V, W, E, G](g, s)
}

/** A singleton for the single source shortest path problem.
 * 
 * @author Łukasz Szpakowski
 */
object ShortestPath
{
  implicit def graphToShortestPath[V, W, E <: WEdgeLike[V, W, _, E], G <: base.GraphBound[V, E, G]](g: base.GraphBound[V, E, G])(implicit num: Numeric[W]) =
    new ShortestPath[V, W, E, base.GraphBound[V, E, G]](g)
}