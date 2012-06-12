/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

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
