/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

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
  
  
  /** The REPR for graph. */
  def graphRepr: G =
    repr
    
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
  
  /** Returns the neighbors for the specified vertex.
   * @param v			the vertex.
   * @return			the neighbors. 
   */
  def neighbors(v: V): Iterable[V] =
    verticesFrom(v).toSet | (if(hasDirectedEdges) verticesTo(v).toSet else Set[V]())
    
  /** Returns the neighbor edges for the specified vertex.
   * @param v			the vertex
   * @return			the neighbor edges.
   */
  def neighborEdges(v: V): Iterable[E[V, X]] =
    edgesFrom(v).toSet | (if(hasDirectedEdges) edgesTo(v).toSet else Set[E[V, X]]())
    
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
  def dfsFrom(s: V): immutable.Tree[V, X, E] = {
    val stck = collection.mutable.Stack[Iterator[E[V, X]]]()
    val visiteds = collection.mutable.Set[V]()
    var t = immutable.Tree[V, X, E](V(s))
    stck.push(edgesFrom(s).iterator)
    visiteds += s
    while(!stck.isEmpty) {
      val iter = stck.head
      if(iter.hasNext) {
        val e = iter.next()
        if(!visiteds(e.out)) {
          stck.push(edgesFrom(e.out).iterator)
          visiteds += e.out
          t = t +~^ e
        }
      } else
        stck.pop()
    }
    t
  }
  
  /** Creates a tree by the DFS algorithm that begins searching for the stack.
   * @param ss			the stack.
   * @return			a trees.
   */
  def dfsFromStack(ss: Seq[V]): Map[V, immutable.Tree[V, X, E]] = {
    val stck = collection.mutable.Stack[(Iterator[E[V, X]], Int)]()
    val visiteds = collection.mutable.Set[V]()
    val ts = ss.map { s => immutable.Tree[V, X, E](V(s)) }.toArray
    for(i <- 0 until ss.size) stck.push((edgesFrom(ss(i)).iterator, i))
    visiteds ++= ss
    while(!stck.isEmpty) {
      val (iter, i) = stck.head
      if(iter.hasNext) {
        val e = iter.next()
        if(!visiteds.contains(e.out)) {
          stck.push((edgesFrom(e.out).iterator, i))
          visiteds += e.out
          ts(i) = ts(i) +~^ e
        }
      } else
        stck.pop()
    }
    ts.map { t => (t.root, t) }.toMap
  }
  
  /** Creates a tree by the BFS algorithm.
   * @param s			the start vertex.
   * @return			a tree.
   */
  def bfsFrom(s: V): immutable.Tree[V, X, E] = {
    val q = collection.mutable.Queue[V]()
    val visiteds = collection.mutable.Set[V]()
    var t = immutable.Tree[V, X, E](V(s))
    q += s
    visiteds += s
    while(!q.isEmpty){
      val v = q.dequeue()
      for(e <- edgesFrom(v)) {
        if(!visiteds.contains(e.out)) {
          q.enqueue(e.out)
          visiteds += e.out
          t = t +~^ e
        }
      }
    }
    t
  }

  /** Creates a tree by the BFS algorithm that begins searching for the queue.
   * @param ss			the queue.
   * @return			a trees.
   */
  def bfsFromQueue(ss: Seq[V]): Map[V, immutable.Tree[V, X, E]] = {
    val q = collection.mutable.Queue[(V, Int)]()
    val visiteds = collection.mutable.Set[V]()
    val ts = ss.map { s => immutable.Tree[V, X, E](V(s)) }.toArray
    for(i <- 0 until ss.size) q.enqueue((ss(i), i))
    visiteds ++= ss
    while(!q.isEmpty) {
      val (v, i) = q.dequeue()
      for(e <- edgesFrom(v)) {
        if(!visiteds.contains(e.out)) {
          q.enqueue((e.out, i))
          visiteds += e.out
          ts(i) = ts(i) +~^ e
        }
      }
    }
    ts.map { t => (t.root, t) }.toMap
  }
  
  /** Checks whether the graph is connected. */
  def isConnected: Boolean = throw new Exception
  
  /** Checks whether the graph is complete. */
  def isComplete: Boolean =
    // This solution is correct if this graph isn't multigraph or hypergraph.
    vertices.forall { neighbors(_).size == vertices.size -1 }
  
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
