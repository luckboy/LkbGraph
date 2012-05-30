package org.lbkgraph.algorithm
import org.lbkgraph._
import org.lbkgraph.immutable._

/** A class for the implementation of Prim's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
class Prim[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W])
{
  /** Finds the minimum spanning tree by Prim's algorithm. */
  def primMinSpanningTree: Option[G] =
    Prim.minSpanningTree[V, W, G](g)

  /** Finds the minimum spanning trees for the connected components by Prim's algorithm. */
  def primMinSpanningTrees: Set[G] = throw new Exception
}

/** A singleton for the implementation of Prim's algorithm.
 * 
 * @author Łukasz Szpakowski
 */
object Prim extends MinSpanningTreeStrategy
{  
  implicit def graphToPrim[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: base.GraphBound[V, Weighted[W], UndiEdge, G])(implicit cmp: Ordering[W]) =
    new Prim[V, W, base.GraphBound[V, Weighted[W], UndiEdge, G]](g)

  override def minSpanningTrees[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: G)(implicit cmp: Ordering[W]): Set[G] =
    new Prim[V, W, G](g).primMinSpanningTrees
}