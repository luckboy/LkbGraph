package org.lkbgraph.immutable
import scala.collection.mutable.Builder
import org.lkbgraph._
import org.lkbgraph.mutable.PathBuilder

/** A factory class for path.
 * 
 * @author Łukasz Szpakowski
 */
abstract class PathFactory[PP[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: Path[XV, XX, XE] with PathLike[XV, XX, XE, _, _, PP[XV, XX, XE]]]
{
  def pathEmpty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): PP[V, X, E]
  
  def newPathBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): Builder[E[V, X], PP[V, X, E]] =
    new PathBuilder(pathEmpty(root))

  /** Creates a new path from the root and edges.
   * @param root		the root.
   * @param es			the edges.
   * @return			a new tree.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V, es: E[V, X]*): Tree[V, X, E] =
    (newPathBuilder(root) ++= es).result
}