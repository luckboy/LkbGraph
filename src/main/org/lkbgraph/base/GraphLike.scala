package org.lkbgraph.base
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A template trait for the graph.
 * 
 * @author Łukasz Szpakowski
 */
trait GraphLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: GraphLike[V, X, E, G] with Graph[V, X, E]] extends collection.SetLike[GraphParam[V, X, E], G]
{  
  /** Creates a new builder for graph. */
  def newGraphBuilder: Builder[GraphParam[V, X, E], G] =
    newBuilder
  
  /** The vertex set. */
  def vertexSet: Set[V] =
    vertices.toSet

  /** The vertices. */
  def vertices: Iterable[V]
  
  /** The iterator of vertices. */
  def verticesIterator: Iterator[V] =
    vertices.toIterator

  /** The edge set. */
  def edgeSet: Set[E[V, X]] =
    edges.toSet

  /** The edges. */
  def edges: Iterable[E[V, X]]
  
  /** The iterator of the edges. */
  def edgesIterator: Iterator[E[V, X]] =
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
  def edgesFrom(s: V): Iterable[E[V, X]] =
    edges.filter { _.in == s }

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
  def edgesTo(t: V): Iterable[E[V, X]] =
    edges.filter { _.out == t }
    
  /** Tests whether the graph contains the specified vertex.
   * @param v			the vertex.
   * @return			the true if the graph contains the vertex, false otherwise.
   */
  def containsVertex(v: V): Boolean =
    vertexSet.contains(v)
  
  /** Tests whether the graph contains the specified edge.
   * @param v			the edge.
   * @return			the true if the graph contains the edge, false otherwise.
   */
  def containsEdge(e: E[V, X]): Boolean =
    edgeSet.contains(e)
  
  /** Returns a copy of the graph with a new vertex.
   * @param v			a new vertex.
   * @return			a copy of the graph.
   */
  def +@ (v: V): G
  
  /** Returns a copy of the graph with a new edge.
   * @param e			a new edge.
   * @return			a copy of the graph.
   */
  def +~ (e: E[V, X]): G

  /** Returns a copy of the graph without the specified vertex.
   * @param v			the vertex.
   * @return			a copy of the graph.
   */
  def -@ (v: V): G
  
  /** Returns a copy of the graph without the specified edge.
   * @param e			the edge.
   * @return			a copy of the graph.
   */
  def -~ (e: E[V, X]): G =
    -~!(e.toUnweightedEdge)
  
  /** Returns a copy of the graph without the specified edge without the specified weight.
   * @param e			the edge.
   * @return			a copy of the graph.
   */
  def -~! (e: E[V, Unweighted]): G

  /** Returns a copy of the graph without the specified vertex or the specified edge.
   * @param param		the vertex or the edge.
   * @return			a copy of the graph.
   */
  def -!(param: GraphParam[V, Unweighted, E]): G =
    (param: @unchecked) match {
      case Vertex(v)           => -@(v)
      case e: E[V, Unweighted] => -~!(e)
    }
  
  /** Returns a copy of the graph without the specified vertices or the specified edges.
   * @param param1		the first vertex or the first edge.
   * @param param2		the second vertices or the second edges.
   * @param params		the vertices or the edges.
   * @return			a copy of the graph.
   */
  def -!(param1: GraphParam[V, Unweighted, E], param2: GraphParam[V, Unweighted, E], params: GraphParam[V, Unweighted, E]*): G =
    this -! param1 -! param2 --! params
    
  /** Returns a copy of the graph without the specified vertices or the specified edges.
   * @param param		the vertices or the edges.
   * @return			a copy of the graph.
   */
  def --!(params: GenTraversableOnce[GraphParam[V, Unweighted, E]]): G =
    params.foldLeft(repr) { _ -! _ }
    
  /** Creates a tree by the DFS algorithm.
   * @param s			the start vertex.
   * @return			a tree.
   */
  def dfsFrom(s: V): immutable.Tree[V, X, E] = throw new Exception
  
  /** Creates a tree by the DFS algorithm that begins searching for the stack.
   * @param ss			the stack.
   * @return			a trees.
   */
  def dfsFromStack(stck: Seq[V]): Map[V, immutable.Tree[V, X, E]] = throw new Exception
  
  /** Creates a tree by the BFS algorithm.
   * @param s			the start vertex.
   * @return			a tree.
   */
  def bfsFrom(s: V): immutable.Tree[V, X, E] = throw new Exception

  /** Creates a tree by the BFS algorithm that begins searching for the queue.
   * @param ss			the queue.
   * @return			a trees.
   */
  def bfsFromQueue(q: Seq[V]): Map[V, immutable.Tree[V, X, E]] = throw new Exception
  
  /** Checks whether the graph is connected. */
  def isConnected: Boolean = throw new Exception
  
  /** Checks whether the graph is complete. */
  def isComplete: Boolean = throw new Exception
  
  /** Creates a transposed graph from the graph. */
  def transposed: G = throw new Exception
    
  /** Returns weak connected components from the graph. */
  def connectedComponents[G1 >: G]: Set[G1] = throw new Exception
    
  /** Converts the graph to a undirected graph. */
  def toUndirectedGraph[G1 <: GraphLike[V, X, UndiEdge, _]](implicit bf: CanBuildFrom[G, GraphParam[V, X, UndiEdge], G1]): G1 =
    map { 
      case Vertex(v)  => Vertex(v)
      case e: E[V, X] => e.toUndirectedEdge
    }
  
  /** Converts the graph a unweighted graph. */
  def toUnweightedGraph[G1 <: GraphLike[V, Unweighted, E, _]](implicit bf: CanBuildFrom[G, GraphParam[V, Unweighted, E], G1]): G1 =
    map {
      case Vertex(v)  => Vertex(v)
      case e: E[V, X] => e.toUnweightedEdge
    }

  /** Returns true if this graph has the directed edges, false otherwise. */
  def hasDirectedEdges: Boolean =
    edgesIterator.take(1).toIterable.headOption.map { _.isDirected }.getOrElse(false)
    
  /** Returns true if this graph has the undirected edges, false otherwise. */
  def hasUndirectedEdges: Boolean =
    edgesIterator.take(1).toIterable.headOption.map { !_.isDirected }.getOrElse(false)

  /** Returns true if this graph has the weighted edges, false otherwise. */
  def hasWeightedEdges: Boolean =
    edgesIterator.take(1).toIterable.headOption.map { _.hasWeight }.getOrElse(false)

  /** Returns true if this graph has the unweighted edges, false otherwise. */
  def hasUnweightedEdges: Boolean =
    edgesIterator.take(1).toIterable.headOption.map { !_.hasWeight }.getOrElse(false)
  
  override def contains(param: GraphParam[V, X, E]): Boolean =
    (param: @unchecked) match {
      case Vertex(v)  => containsVertex(v)
      case e: E[V, X] => containsEdge(e)
    }
  
  override def iterator: Iterator[GraphParam[V, X, E]] =
    verticesIterator.map { Vertex(_) } ++ edgesIterator

  override def + (param: GraphParam[V, X, E]): G =
    (param: @unchecked) match {
      case Vertex(v)  => +@(v)
      case e: E[V, X] => +~(e)
    }

  override def - (param: GraphParam[V, X, E]): G =
    (param: @unchecked) match {
      case Vertex(v)  => -@(v)
      case e: E[V, X] => -~(e)
    }
}