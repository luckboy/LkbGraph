package org.lbkgraph.algorithm
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A class for the minimum spanning tree.
 * 
 * @author Łukasz Szpakowski
 */
class MinSpanningTree[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W])
{
  /** Finds the minimum spanning tree. */
  def minSpanningTree(implicit strategy: MinSpanningTreeStrategy): Option[G] =
    strategy.minSpanningTree[V, W, G](g)

  /** Finds the minimum spanning trees for the connected components. */
  def minSpanningTrees(implicit strategy: MinSpanningTreeStrategy): Set[G] =
    strategy.minSpanningTrees[V, W, G](g)
}

/** A singleton for the minimum spanning tree.
 * 
 * @author Łukasz Szpakowski
 */
object MinSpanningTree
{
  def minSpanningTreeToGraph[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: base.GraphBound[V, Weighted[W], UndiEdge, G])(implicit cmp: Ordering[W]) =
    new MinSpanningTree[V, W, base.GraphBound[V, Weighted[W], UndiEdge, G]](g)
}