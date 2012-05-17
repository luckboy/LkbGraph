package org.lbkgraph.algorithm
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A class for the implementation of Kruskal's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
class Kruskal[V, W, G <: base.GraphBound[V, WUndiEdge[V, W], G]](g: G)(implicit cmp: Ordering[W])
{
  /** Finds the minimum spanning tree by Kruskal's algorithm. */
  def kruskalMinSpanningTree: Option[G] = throw new Exception

  /** Finds the minimum spanning trees for the connected components by Kruskal's algorithm. */
  def kruskalMinSpanningTrees: Set[G] = throw new Exception
}

/** A singleton for the implementation of Kruskal's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
object Kruskal extends MinSpanningTreeStrategy
{  
  implicit def graphToKruskal[V, W, G  <: base.GraphBound[V, WUndiEdge[V, W], G]](g: base.GraphBound[V, WUndiEdge[V, W], G])(implicit cmp: Ordering[W]) =
    new Kruskal[V, W, base.GraphBound[V, WUndiEdge[V, W], G]](g)

  override def minSpanningTree[V, W, G <: base.GraphBound[V, WUndiEdge[V, W], G]](g: G)(implicit cmp: Ordering[W]): Option[G] =
    new Kruskal[V, W, G](g).kruskalMinSpanningTree

  override def minSpanningTrees[V, W, G <: base.GraphBound[V, WUndiEdge[V, W], G]](g: G)(implicit cmp: Ordering[W]): Set[G] =
    new Kruskal[V, W, G](g).kruskalMinSpanningTrees
}