package org.lkbgraph.immutable
import org.lkbgraph._

/** A template trait for immutable path.
 * 
 * @author Łukasz Szpakowski
 */
trait PathLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: base.GraphLike[V, X, E, G] with Graph[V, X, E], +T <: TreeLike[V, X, E, G, T] with Tree[V, X, E], +P <: PathLike[V, X, E, G, T, P] with Path[V, X, E]] extends TreeLike[V, X, E, G, T]
{
  /** The vertex sequence from the first vertex to the last vertex. */
  def vertexSeq: Iterable[V]
  
  /** The edge sequence from the first edge to the last edge. */
  def edgeSeq: Iterable[E[V, X]]
  
  /** This methods is like vertexSeq. */
  def nodeSeq: Iterable[V] =
    vertexSeq
    
  /** Returns a copy of this path with a new edge if the input vertex is end point of this path.
   * @param e		the edge.
   * @return		a copy of this path.
   */
  def +~/ (e: E[V, X]): P
  
  /** Returns a copy of this path without the line segment from the specified vertex.
   * @param v		the vertex.
   * @return		a copy of this path.
   */
  def -@/ (v: V): P
}