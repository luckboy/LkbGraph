package org.lbkgraph.base
import org.lbkgraph._

trait TreeLike[V, E <: EdgeLike[V, E], +T <: TreeLike[V, E, T] with Tree[V, E]] extends GraphLike[V, E, T] with Graph[V, E]
{
  /** The nodes is the synonym of vertices. */
  def nodes: Set[V]
  
  /** The root. */
  def root: V
  
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