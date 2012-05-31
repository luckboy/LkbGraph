package org.lkbgraph.immutable
import org.lkbgraph._

/** A template trait for immutable path.
 * 
 * @author Łukasz Szpakowski
 */
trait PathLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: base.GraphLike[V, X, E, G] with Graph[V, X, E], +P <: PathLike[V, X, E, G, P] with Path[V, X, E]] extends TreeLike[V, X, E, G, P]
{
  /** The vertex sequence from the first vertex to the last vertex. */
  def vertexSeq: Seq[V]
  
  /** The edge sequence from the first edge to the last edge. */
  def edgeSeq: Seq[E[V, X]]
  
  /** This methods is like vertexSeq. */
  def nodeSeq: Seq[V] =
    vertexSeq
}