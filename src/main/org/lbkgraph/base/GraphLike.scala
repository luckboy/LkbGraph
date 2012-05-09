package org.lbkgraph.base
import org.lbkgraph._

trait GraphLike[V, E <: EdgeLike[V, E], +T <: GraphLike[V, E, T] with Graph[V, E]] extends collection.SetLike[GraphParam[V, E], T] with collection.Set[GraphParam[V, E]]
{
  override def empty: T = throw new Exception
  
  override def newBuilder: collection.mutable.Builder[GraphParam[V, E], T] = throw new Exception
  
  /** The vertices. */
  def vertices: Set[V]

  /** The edges. */
  def edges: Set[E]
  
  /** Returns the source vertices of the specified vertice. 
   * @param s			the start vertice.
   * @return			the source vertices.
   */
  def verticesFrom(s: V): Set[V]
  
  /** Returns the edges from the specified vertice.
   * @param s			the start vertice.
   * @return			the edges.
   */
  def edgesFrom(s: V): Set[E]

  /** Returns the destination vertices of the specified vertice. 
   * @param s			the end vertice.
   * @return			the destination vertices.
   */
  def verticesTo(t: V): Set[V]
  
  /** Returns the edges to the specified vertice.
   * @param s			the end vertice.
   * @return			the edges.
   */
  def edgesTo(t: V): Set[E]
    
  /** Creates a tree by the DFS algorithm.
   * @param s			the start vertice.
   * @return			a tree.
   */
  def dfsFrom(s: V): immutable.Tree[V, E] = throw new Exception
  
  /** Creates a tree by the BFS algorithm.
   * @param s			the start vertice.
   * @return			a tree.
   */
  def bfsFrom(s: V): immutable.Tree[V, E] = throw new Exception
  
  /** Creates a tree by the BFS algorithm that begins searching from many sources.
   * @param ss			the start vertices.
   * @return			a trees.
   */
  def bfsFromN(ss: Seq[V]): Seq[immutable.Tree[V, E]] = throw new Exception
  
  /** Checks whether the graph is connected. */
  def isConnected: Boolean = throw new Exception
  
  /** Checks whether the graph is complete. */
  def isComplete: Boolean = throw new Exception
  
  /** Creates a transposed graph from the graph. */
  def transposed: T = throw new Exception
  
  /** Returns weak connected components from the graph. */
  def connectedComponents[U >: T]: Set[U] = throw new Exception
}