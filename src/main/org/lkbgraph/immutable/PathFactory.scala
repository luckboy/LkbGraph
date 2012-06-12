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
import org.lkbgraph.mutable.PathBuilder

/** A factory class for path.
 * 
 * @author Łukasz Szpakowski
 */
abstract class PathFactory[PP[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: Path[XV, XX, XE] with PathLike[XV, XX, XE, _, _, PP[XV, XX, XE]]]
{
  def pathEmpty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): PP[V, X, E]
  
  def newPathBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): Builder[E[V, X], PP[V, X, E]] =
    new PathBuilder(pathEmpty(root))

  /** Creates a new path from the root and edges.
   * @param root		the root.
   * @param es			the edges.
   * @return			a new path.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: Vertex[V], es: E[V, X]*): Tree[V, X, E] =
    (newPathBuilder(root.value) ++= es).result
}
