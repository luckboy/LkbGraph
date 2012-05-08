package org.lbkgraph

trait GraphLike[+V, +E <: Product2[V, V], +T] 
{
  /** The graph vertices. */
  def vertices: Set[V]

  /** The graph edges. */
  def edges: Set[E]
  
  /** Returns a copy of the graph with the new vertex.
   * @param v			the new vertex.
   * @return			a new graph.
   */
  def + (v: V): T
    
  /** Returns a copy of the graph without the vertex. 
   * @param v			the vertex.
   * @return			a new graph.
   */
  def - (v: V): T

  /** Returns a copy of the graph with the new vertices.
   * @param v1			the first vertex.
   * @param v2			the second vertex.
   * @param vs			the another vertices.
   * @return			a new graph.
   */
  def + (v1: V, v2: V, vs: V*): T = throw new Exception
    
  /** Returns a copy of the graph without the vertices. 
   * @param v1			the first vertex.
   * @param v2			the second vertex.
   * @param vs			the another vertices.
   * @return			a new graph.
   */
  def - (v1: V, v2: V, vs: V*): T = throw new Exception
  
  /** Returns a copy of the graph with the new edge. Any vertex of new edge can be non-existing at the graph.
   * @param v			the new edge.
   * @return			a new graph.
   */
  def withEdge(e: E): T
  
  /** Returns a copy of the graph without the edge.
   * @param v			the edge.
   * @return			a new graph.
   */
  def withoutEdge(e: E): T

  /** Calculates a union of graphs.
   * @param g			the second graph.
   * @return			a union of graphs.
   */
  def union(g: Graph[V, E]): T = throw new Exception
  
  /** Calculates a intersect of graphs.
   * @param g			the second graph.
   * @return			a intersect of graphs.
   */
  def intersect(g: Graph[V, E]): T = throw new Exception 
  
  /** Calculates a difference of graphs.
   * @param g			the second graph.
   * @return			a difference of graphs.
   */
  def diff(g: Graph[V, E]): T = throw new Exception  
  
  /** Calculates a union of graphs. */
  def | (g: Graph[V, E]): T = 
    union(g)

  /** Calculates a intersect of graphs. */
  def & (g: Graph[V, E]): T = 
    intersect(g)
  
  /** Calculates a difference of graphs. */
  def &~ (g: Graph[V, E]): T = 
    diff(g)
  
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
}