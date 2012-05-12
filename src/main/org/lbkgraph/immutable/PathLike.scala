package org.lbkgraph.immutable
import org.lbkgraph._

/** A template trait for the immutable path.
 * 
 * @author Łukasz Szpakowski
 */
trait PathLike[V, E <: EdgeLike[V, E], +G <: base.GraphLike[V, E, G] with Graph[V, E], +P <: PathLike[V, E, G, P] with Path[V, E]] extends TreeLike[V, E, G, P]
{
  /** The vertex sequence from the first vertex to the last vertex. */
  def vertexSeq: Seq[V]
  
  /** The edge sequence from the first edge to the last edge. */
  def edgeSeq: Seq[E]
  
  /** This methods is like vertexSeq. */
  def nodeSeq: Seq[V] =
    vertexSeq
}