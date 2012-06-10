package org.lkbgraph.immutable
import scala.collection.mutable.Builder
import org.lkbgraph._
import org.lkbgraph.mutable.TreeBuilder

/** A factory class for tree.
 * 
 * @author Łukasz Szpakowski
 */
abstract class TreeFactory[TT[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: Tree[XV, XX, XE] with TreeLike[XV, XX, XE, _, TT[XV, XX, XE]]]
{
  def treeEmpty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): TT[V, X, E]
  
  def newTreeBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): Builder[E[V, X], TT[V, X, E]] =
    new TreeBuilder(treeEmpty(root))
  
  /** Creates a new tree from the root and edges.
   * @param root		the root.
   * @param es			the edges.
   * @return			a new tree.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: Vertex[V], es: E[V, X]*): Tree[V, X, E] =
    (newTreeBuilder(root.value) ++= es).result
}