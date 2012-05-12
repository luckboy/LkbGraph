package org.lbkgraph.immutable
import org.lbkgraph._

/** A template trait for the immutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait TreeLike[V, E <: EdgeLike[V, E], +G <: base.GraphLike[V, E, G] with Graph[V, E], +T <: TreeLike[V, E, G, T] with Tree[V, E]] extends ForestLike[V, E, G, T]
{  
  /** The root. */
  def root: V
  
  /** The branches from the root. */
  def branches: Set[Tree[V, E]] =
    branchesFrom(root)
    
  /** The children from the root. */
  def childs: Set[V] =
    childsFrom(root)
    
  /** All paths from the root to the leaf. */
  def paths: Set[Path[V, E]] = throw new Exception

  /** All paths from the specified node to the leaf.
   * @param s			the node.
   * @return			the paths.
   */
  def pathsFrom(s: V): Set[Path[V, E]] = throw new Exception

  /** The pre-order traversal sequence. */
  def preOrder: Seq[V] =
    preOrderFrom(root)
    
  /** The post-order traversal sequence. */
  def postOrder: Seq[V] =
    postOrderFrom(root)

  /** The level-order traversal sequence. */
  def levelOrder: Seq[V] =
    preOrderFrom(root)
}