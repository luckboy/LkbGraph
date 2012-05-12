package org.lbkgraph.immutable
import org.lbkgraph._

/** A template trait for the immutable forest.
 * 
 * @author Łukasz Szpakowski
 */
trait ForestLike[V, E <: EdgeLike[V, E], +G <: base.GraphLike[V, E, G] with Graph[V, E], +F <: ForestLike[V, E, G, F] with Forest[V, E]] extends base.GraphLike[V, E, G]
{
  /** The nodes is the synonym of vertices. */
  def nodes: Set[V] =
    vertices
  
  /** The branches from the specified node. 
   * @param s			the start node.
   * @return			the branches.
   */
  def branchesFrom[T1 >: Tree[V, E]](s: V): Set[T1]

  /** The children from the specified node. 
   * @param s			the start node.
   * @return			the childs.
   */
  def childsFrom(s: V): Set[V]

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