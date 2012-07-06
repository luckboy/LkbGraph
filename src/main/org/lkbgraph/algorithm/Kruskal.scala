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
import org.lkbgraph.mutable.DisjointSet

/** A class for the implementation of Kruskal's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
class Kruskal[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W])
{
  import cmp._
  
  /** Finds the minimum spanning tree by Kruskal's algorithm. */
  def kruskalMinSpanningTree: Option[G] = 
    Kruskal.minSpanningTree[V, W, G](g)

  /** Finds the minimum spanning trees for the connected components by Kruskal's algorithm. */
  def kruskalMinSpanningTrees: Set[G] = {
    val es = g.edges.toSeq.sortWith { (e1, e2) => e1.weight < e1.weight }
    val dsns = g.vertices.map { v => (v, DisjointSet.Node(())) }.toMap
    val fls = collection.mutable.Map() ++ g.vertices.map { 
      (_, collection.mutable.DoubleLinkedList[UndiEdge[V, Weighted[W]]]())
    }
    for(e <- es) {
      val ds1 = dsns(e._1).toDisjointSet
      val ds2 = dsns(e._2).toDisjointSet
      // Whether does an edge connect two unconnected trees?
      if(ds1 != ds2) {
        // An edge connects two unconnected trees and then there connect two unconnected trees into one tree.
        ds1 |= ds2
        val fs = fls(e._1)
        fs.insert(fls(e._2))
        fls(e._1) = fs
        fls(e._2) = fs
      }
    }
    fls.values.map { fs => (g.newGraphBuilder ++= fs).result }.toSet
  }
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
