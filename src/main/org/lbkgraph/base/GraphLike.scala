package org.lbkgraph.base
import org.lbkgraph._

/** A template trait for the graph.
 * 
 * @author Łukasz Szpakowski
 */
trait GraphLike[V, E <: EdgeLike[V, E], +G <: GraphLike[V, E, G] with Graph[V, E]] extends collection.SetLike[GraphParam[V, E], G]
{  
  /** The vertex set. */
  def vertexSet: Set[V] =
    vertices.toSet

  /** The vertices. */
  def vertices: Iterable[V]
  
  /** The vertex iterator. */
  def vertexIterator: Iterator[V] =
    vertices.toIterator

  /** The edge set. */
  def edgeSet: Iterable[E] =
    edges.toSet

  /** The edges. */
  def edges: Iterable[E]
  
  /** The edge iterator. */
  def edgeIterator: Iterator[E] =
    edges.toIterator
  
  /** Returns the output vertices of the specified vertex. 
   * @param s			the start vertex.
   * @return			the output vertices.
   */
  def verticesFrom(s: V): Iterable[V] =
    edgesFrom(s).map { _.out }
  
  /** Returns the edges from the specified vertex.
   * @param s			the start vertex.
   * @return			the edges.
   */
  def edgesFrom(s: V): Iterable[E]

  /** Returns the input vertices of the specified vertex. 
   * @param s			the end vertex.
   * @return			the input vertices.
   */
  def verticesTo(t: V): Iterable[V] =
    edgesFrom(t).map { _.in }
  
  /** Returns the edges to the specified vertex.
   * @param s			the end vertex.
   * @return			the edges.
   */
  def edgesTo(t: V): Iterable[E] =
    edges.filter { _.out == t }
    
  /** Finds the specified vertex.
   * @param v			the edge.
   * @return			the found vertex.
   */
  def findVertex(v: V): Option[V] =
    vertices.find(v ==)
  
  /** Finds the specified edge.
   * @param e			the edge.
   * @param f			the convert function.
   * @return			the found edge.
   */
  def findEdge[E1 <: EdgeLike[V, _], E2](e: E1)(implicit cf: CanFind[Iterable[E], E2, E1]): Option[E2] =
    cf.find(edges)(e)
    
  /** Creates a tree by the DFS algorithm.
   * @param s			the start vertex.
   * @return			a tree.
   */
  def dfsFrom(s: V): immutable.Tree[V, E] = throw new Exception
  
  /** Creates a tree by the DFS algorithm that begins searching for the stack.
   * @param ss			the stack.
   * @return			a trees.
   */
  def dfsFromStack(stck: Seq[V]): Map[V, immutable.Tree[V, E]] = throw new Exception
  
  /** Creates a tree by the BFS algorithm.
   * @param s			the start vertex.
   * @return			a tree.
   */
  def bfsFrom(s: V): immutable.Tree[V, E] = throw new Exception

  /** Creates a tree by the BFS algorithm that begins searching for the queue.
   * @param ss			the queue.
   * @return			a trees.
   */
  def bfsFromQueue(q: Seq[V]): Map[V, immutable.Tree[V, E]] = throw new Exception
  
  /** Checks whether the graph is connected. */
  def isConnected: Boolean = throw new Exception
  
  /** Checks whether the graph is complete. */
  def isComplete: Boolean = throw new Exception
  
  /** Creates a transposed graph from the graph. */
  def transposed: G = throw new Exception
    
  /** Returns weak connected components from the graph. */
  def connectedComponents[G1 >: G]: Set[G1] = throw new Exception
}