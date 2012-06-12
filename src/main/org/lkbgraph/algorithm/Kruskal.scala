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
import org.lkbgraph._
import org.lkbgraph.immutable._

/** A class for the implementation of Kruskal's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
class Kruskal[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W])
{
  /** Finds the minimum spanning tree by Kruskal's algorithm. */
  def kruskalMinSpanningTree: Option[G] = 
    Kruskal.minSpanningTree[V, W, G](g)

  /** Finds the minimum spanning trees for the connected components by Kruskal's algorithm. */
  def kruskalMinSpanningTrees: Set[G] = throw new Exception
}

/** A singleton for the implementation of Kruskal's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
object Kruskal extends MinSpanningTreeStrategy
{  
  implicit def graphToKruskal[V, W, G  <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: base.GraphBound[V, Weighted[W], UndiEdge, G])(implicit cmp: Ordering[W]) =
    new Kruskal[V, W, base.GraphBound[V, Weighted[W], UndiEdge, G]](g)

  override def minSpanningTrees[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W]): Set[G] =
    new Kruskal[V, W, G](g).kruskalMinSpanningTrees
}
