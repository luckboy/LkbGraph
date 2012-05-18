package org.lbkgraph.algorithm
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A trait for the strategy of the minimum spanning tree.
 * 
 * @author Łukasz Szpakowski
 */
trait MinSpanningTreeStrategy
{
  def minSpanningTree[V, W, G <: base.GraphBound[V, WUndiEdge[V, W], G]](g: G)(implicit cmp: Ordering[W]): Option[G] = {
    val mst = minSpanningTrees[V, W, G](g)
    if(mst.size == 1) mst.firstOption else None
  }

  def minSpanningTrees[V, W, G <: base.GraphBound[V, WUndiEdge[V, W], G]](g: G)(implicit cmp: Ordering[W]): Set[G]
}