package org.lkbgraph.immutable
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A template trait for immutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait TreeLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: base.GraphLike[V, X, E, G] with Graph[V, X, E], +T <: TreeLike[V, X, E, G, T] with Tree[V, X, E]] extends base.GraphLike[V, X, E, G]
{
  /** Creates a tree builder for graph.
   * @param	root		the root.
   * @return			a new builder for graph.
   */
  def newTreeBuilder(root: V): Builder[E[V, X], T]
  
  /** The nodes is the synonym of vertexSet. */
  def nodeSet: Iterable[V] =
    vertexSet
  
  /** The nodes is the synonym of vertices. */
  def nodes: Iterable[V] =
    vertices

  /** The nodes is the synonym of verticesIterator. */
  def nodesIterator: Iterator[V] =
    verticesIterator

  /** The root. */
  def root: V

  /** The branches from the root. */
  def branches: Iterable[Tree[V, X, E]] =
    branchesFrom(root)
    
  /** The branches from the specified node. 
   * @param s			the start node.
   * @return			the branches.
   */
  def branchesFrom(s: V): Iterable[Tree[V, X, E]] = throw new Exception

  /** The children from the root. */
  def children: Iterable[V] =
    childrenFrom(root)
    
  /** The children from the specified node. 
   * @param s			the start node.
   * @return			the children.
   */
  def childrenFrom(s: V): Iterable[V] =
    childEdgesFrom(s).map { _.out }
  
  /** The child edges from the root. */
  def childEdges: Iterable[E[V, X]] =
    childEdgesFrom(root)
  
  /** The child edges the specified node. 
   * @param s			the start node.
   * @return			the child edges.
   */
  def childEdgesFrom(s: V): Iterable[E[V, X]]

  /** Returns a copy of the tree with the specified edge if either of two edge vertices is exists at the tree. In case 
   * the edge is directed, the input vertex of the edge should be in the tree and the output vertex shouldn't to be at 
   * the tree. If the following condition isn't satisfied, the edge don't add to a copy of the tree.
   * @param e			the edge.
   * @return 			a copy of the tree with the specified edge.
   */
  def +~^ (e: E[V, X]): T

  /** Returns a copy of the tree without the branch that from the specified node.
   * @param s			the node.
   * @return			a copy of the tree without the branch.
   */
  def -@^ (s: V): T  
  
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
  
  override def edgesFrom(s: V): Iterable[E[V, X]] =
    childEdgesFrom(s) ++ (if(hasUndirectedEdges) edges.filter { _.out == s } else Nil)
    
  override def toString: String =
    stringPrefix + "(" + root + "," + edges.mkString(",") + ")"

  override def stringPrefix: String =
    "Tree"
}