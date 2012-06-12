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

package org.lkbgraph

package object base
{  
  type GraphBound[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E] , +G <: GraphLike[V, X, E, G] with Graph[V, X, E]] = GraphLike[V, X, E, G] with Graph[V, X, E]

  type UnwGraphBound[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E] , +G <: GraphLike[V, Unweighted, E, G] with Graph[V, Unweighted, E]] = GraphLike[V, Unweighted, E, G] with Graph[V, Unweighted, E]
  
  type WGraphBound[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E] , +G <: GraphLike[V, Weighted[W], E, G] with Graph[V, Weighted[W], E]] = GraphLike[V, Weighted[W], E, G] with Graph[V, Weighted[W], E]
}
