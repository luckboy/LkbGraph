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

package org.lkbgraph.immutable
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A template trait for immutable path.
 * 
 * @author Łukasz Szpakowski
 */
trait PathLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: base.GraphLike[V, X, E, G] with Graph[V, X, E], +T <: TreeLike[V, X, E, G, T] with Tree[V, X, E], +P <: PathLike[V, X, E, G, T, P] with Path[V, X, E]] extends TreeLike[V, X, E, G, T]
{
  /** Creates a new builder for path.
   * @param	root		the root.
   * @return			a new builder for path.
   */
  def newPathBuilder(root: V): Builder[E[V, X], P]

  /** The REPR for tree. */
  def pathRepr: P =
    asInstanceOf[P]

  /** The vertex sequence from the first vertex to the last vertex. */
  def vertexSeq: Seq[V] =
    Seq(root) ++ edgeSeq.map { _.out }
  
  /** The edge sequence from the first edge to the last edge. */
  def edgeSeq: Seq[E[V, X]]
  
  /** This methods is like vertexSeq. */
  def nodeSeq: Seq[V] =
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
  
  override def childEdgesFrom(s: V): Iterable[E[V, X]] =
    edgeSeq.find { _.in == s }

  override def +~^ (e: E[V, X]): T =
    (newTreeBuilder(root) ++= edges).result +~^ e
    
  override def -@^ (v: V): T =
    (newTreeBuilder(root) ++= edges).result -@^ v
  
  override def vertices: Iterable[V] =
    vertexSeq
    
  override def edges: Iterable[E[V, X]] =
    edgeSeq
    
  override def stringPrefix: String =
    "Path"
}
