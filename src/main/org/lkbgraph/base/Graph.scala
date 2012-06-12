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

/** A trait for immutable graph and / or  mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends GraphLike[V, X, E, Graph[V, X, E]] with collection.Set[GraphParam[V, X, E]]
{
  override def empty: Graph[V, X, E] =
    immutable.Graph.empty
  
  override protected[this] def newBuilder: Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    immutable.Graph.newBuilder
}

/** A trait for immutable graph and / or  mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
object Graph extends GraphFactory[Graph]
{
  implicit def canBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[Graph[_, _, E1], GraphParam[V, X, E], Graph[V, X, E]] =
    graphCanBuildFrom

  override def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Graph[V, X, E] = 
    AdjListGraph.empty

  override def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    AdjListGraph.newBuilder
}
