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
import scala.collection.mutable.SetBuilder
import org.lkbgraph._

/** A factory class for immutable graph. 
 * 
 * @author Łukasz Szpakowski
 */
abstract class GraphFactory[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: base.GraphLike[XV, XX, XE, GG[XV, XX, XE]] with Graph[XV, XX, XE]] extends base.GraphFactory[GG] 
{
  override def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: Builder[GraphParam[V, X, E], GG[V, X, E]] =
    new SetBuilder(empty)
}
