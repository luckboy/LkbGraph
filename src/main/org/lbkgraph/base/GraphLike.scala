package org.lbkgraph.base
import org.lbkgraph._

/** A template trait for the graph.
 * 
 * @author Łukasz Szpakowski
 */
trait GraphLike[V, E <: EdgeLike[V, E], +G <: GraphLike[V, E, G] with Graph[V, E]] extends collection.SetLike[GraphParam[V, E], G]
{  
  /** The vertices. */
  def vertices: Set[V]

  /** The edges. */
  def edges: Set[E]
  
  /** Returns the destination vertices of the specified vertex. 
   * @param s			the start vertex.
   * @return			the destination vertices.
   */
  def verticesFrom(s: V): Set[V] =
    edgesFrom(s).map { _.dest }
  
  /** Returns the edges from the specified vertex.
   * @param s			the start vertex.
   * @return			the edges.
   */
  def edgesFrom(s: V): Set[E]

  /** Returns the source vertices of the specified vertex. 
   * @param s			the end vertex.
   * @return			the source vertices.
   */
  def verticesTo(t: V): Set[V] =
    edgesFrom(t).map { _.source }
  
  /** Returns the edges to the specified vertex.
   * @param s			the end vertex.
   * @return			the edges.
   */
  def edgesTo(t: V): Set[E]
    
  /** Finds the specified vertex.
   * @param v			the edge.
   * @return			the found vertex.
   */
  def findVertex(v: V): Option[V] =
    vertices.find(v ==)
  
  /** Finds the specified edge.
   * @param e			the edge.
   * @return			the found edge.
   */
  def findEdge(e: Edge[V]): Option[E] =
    edges.find { f => f._1 == e._1 && f._2 == e._2 }
  
  /** Creates a tree by the DFS algorithm.
   * @param s			the start vertex.
   * @return			a tree.
   */
  def dfsFrom(s: V): immutable.Tree[V, E] = throw new Exception
  
  /** Creates a tree by the BFS algorithm.
   * @param s			the start vertex.
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
  def transposed: G = throw new Exception
  
  /** Returns weak connected components from the graph. */
  def connectedComponents[G1 >: G]: Set[G1] = throw new Exception
}