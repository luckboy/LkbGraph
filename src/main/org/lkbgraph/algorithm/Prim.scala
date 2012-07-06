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

/** A class for the implementation of Prim's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
class Prim[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W])
{
  import cmp._
  
  /** Finds the minimum spanning tree by Prim's algorithm. */
  def primMinSpanningTree: Option[G] =
    if(!g.isEmpty) {
      val h = primMinSpanningTreeFrom(g.vertices.head)
      if(h.vertices.size == g.vertices.size) Some(h) else None
    } else
      None

  /** Finds the minimum spanning trees for the connected components by Prim's algorithm. */
  def primMinSpanningTrees: Set[G] = {
    var vs = collection.mutable.Set[V]() ++ g.vertices
    var hs = collection.mutable.Set[G]()
    while(!vs.isEmpty) {
      val h = primMinSpanningTreeFrom(vs.head)
      vs --= h.vertices
      hs += h
    }
    hs.toSet
  }
  
  /** Finds the minimum spanning tree by Prim's algorithm from the specified vertex. */
  def primMinSpanningTreeFrom(s: V): G = {
    val b = g.newGraphBuilder
    if(g.containsVertex(s)) {
      val n = g.vertices.size
      val q = collection.mutable.PriorityQueue[UndiEdge[V, Weighted[W]]]()(new Ordering[UndiEdge[V, Weighted[W]]] {
        override def compare(e1: UndiEdge[V, Weighted[W]], e2: UndiEdge[V, Weighted[W]]): Int =
          -cmp.compare(e1.weight, e2.weight)
      })
      val vs = collection.mutable.Set[V](s)
      b += V(s)
      for(e <- g.edgesFrom(s)) {
        q.enqueue(e)
        vs += e.out
        b += e
      }
      while(!q.isEmpty) {
        val v = q.dequeue().out 	// the vertex that has minimum weight
        val es = g.edgesFrom(v).filterNot { e => vs.contains(e.out) }
        if(!es.isEmpty) {
          val minE = es.minBy { _.weight }
          q.enqueue(minE)
          vs += minE.out
          b += minE
        }
      }
    }
    b.result
  }
}

/** A singleton for the implementation of Prim's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
object Prim extends MinSpanningTreeStrategy
{  
  implicit def graphToPrim[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: base.GraphBound[V, Weighted[W], UndiEdge, G])(implicit cmp: Ordering[W]) =
    new Prim[V, W, base.GraphBound[V, Weighted[W], UndiEdge, G]](g)

  override def minSpanningTree[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W]): Option[G] =
    new Prim[V, W, G](g).primMinSpanningTree
    
  override def minSpanningTrees[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W]): Set[G] =
    new Prim[V, W, G](g).primMinSpanningTrees
}
