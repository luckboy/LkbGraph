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
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for the implementation of the incidence set representation.
 * 
 * @author Łukasz Szpaklowski
 */
trait IncSetGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends IncSetGraphLike[V, X, E, IncSetGraph[V, X, E]] with Graph[V, X, E]
{
  override def empty: IncSetGraph[V, X, E] =
    IncSetGraph.empty
    
  override protected[this] def newBuilder: Builder[GraphParam[V, X, E], IncSetGraph[V, X, E]] =
    IncSetGraph.newBuilder
    
  override def stringPrefix: String =
    "IncSetGraph"
}

/** A singleton for the implementation of the incidence set representation.
 * 
 * @author Łukasz Szpaklowski
 */
object IncSetGraph extends GraphFactory[IncSetGraph]
{
  implicit def canBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[IncSetGraph[_, _, E1], GraphParam[V, X, E], IncSetGraph[V, X, E]] =
    graphCanBuildFrom

  override def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: IncSetGraph[V, X, E] = 
    immutable.IncSetGraph.empty

  override def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Builder[GraphParam[V, X, E], IncSetGraph[V, X, E]] =
    immutable.IncSetGraph.newBuilder
}
