package org.lbkgraph.base
import org.lbkgraph._

/** A trait for the immutable tree and mutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait TreeLike[V, E <: EdgeLike[V, E], +G <: GraphLike[V, E, G] with Graph[V, E], +T <: TreeLike[V, E, G, T] with Tree[V, E]] extends GraphLike[V, E, G]
{
  /** The nodes is the synonym of vertices. */
  def nodes: Set[V] =
    vertices
  
  /** The root. */
  def root: V
  
  /** The branches from the root. */
  def branches[T1 >: T]: Set[T1]
  
  /** The branches from the specified node. */
  def branchesFrom[T1 >: T](s: V): Set[T1]
  
  /** The children from the root. */
  def childs: Set[V]
  
  /** The children from the specified node. */
  def childsFrom(s: V): Set[V]
  
  /** The pre-order traversal sequence. */
  def preOrder: Seq[V] =
    preOrderFrom(root)
    
  /** The post-order traversal sequence. */
  def postOrder: Seq[V] =
    postOrderFrom(root)

  /** The level-order traversal sequence. */
  def levelOrder: Seq[V] =
    preOrderFrom(root)

  /** The pre-order traversal sequence from the specified node. 
   * @param s			the start node.
   * @return			a sequence.
   */
  def preOrderFrom(s: V): Seq[V] = throw new Exception
    
  /** The post-order traversal sequence from the specified node.
   * @param s			the start node.
   * @return			a sequence.
   */
  def postOrderFrom(s: V): Seq[V] = throw new Exception

  /** The level-order traversal sequence from the specified node.
   * @param s			the start node.
   * @return			a sequence.
   */
  def levelOrderFrom(s: V): Seq[V] = throw new Exception
}