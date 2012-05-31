package org.lkbgraph.immutable
import org.lkbgraph._

/** A template trait for immutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait TreeLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: base.GraphLike[V, X, E, G] with Graph[V, X, E], +T <: TreeLike[V, X, E, G, T] with Tree[V, X, E]] extends base.GraphLike[V, X, E, G]
{
  /** The nodes is the synonym of vertexSet. */
  def nodeSet: Iterable[V] =
    vertexSet
  
  /** The nodes is the synonym of vertices. */
  def nodes: Iterable[V] =
    vertices

  /** The nodes is the synonym of vertexSet. */
  def nodeIterator: Iterator[V] =
    vertexIterator

  /** The root. */
  def root: V

  /** The branches from the root. */
  def branches: Set[Tree[V, X, E]] =
    branchesFrom(root)
    
  /** The branches from the specified node. 
   * @param s			the start node.
   * @return			the branches.
   */
  def branchesFrom(s: V): Set[Tree[V, X, E]]

  /** The children from the root. */
  def children: Set[V] =
    childrenFrom(root)
    
  /** The children from the specified node. 
   * @param s			the start node.
   * @return			the children.
   */
  def childrenFrom(s: V): Set[V]

  /** The pre-order traversal sequence. */
  def preOrder: Seq[V] =
    preOrderFrom(root)
    
  /** The pre-order traversal sequence from the specified node. 
   * @param s			the start node.
   * @return			a sequence.
   */
  def preOrderFrom(s: V): Seq[V] = throw new Exception

  /** The post-order traversal sequence. */
  def postOrder: Seq[V] =
    postOrderFrom(root)

  /** The post-order traversal sequence from the specified node.
   * @param s			the start node.
   * @return			a sequence.
   */
  def postOrderFrom(s: V): Seq[V] = throw new Exception

  /** The level-order traversal sequence. */
  def levelOrder: Seq[V] =
    preOrderFrom(root)
    
  /** The level-order traversal sequence from the specified node.
   * @param s			the start node.
   * @return			a sequence.
   */
  def levelOrderFrom(s: V): Seq[V] = throw new Exception
}