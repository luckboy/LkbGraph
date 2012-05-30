package org.lkbgraph.algorithm
import org.lkbgraph._
import org.lkbgraph.immutable._

/** A trait for the strategy of the minimum spanning tree.
 * 
 * @author Łukasz Szpakowski
 */
trait MinSpanningTreeStrategy
{
  def minSpanningTree[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W]): Option[G] = {
    val mst = minSpanningTrees[V, W, G](g)
    if(mst.size == 1) mst.firstOption else None
  }

  def minSpanningTrees[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W]): Set[G]
}