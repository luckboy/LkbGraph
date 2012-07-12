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

package object algorithm
{
  // MinSpanningTree  
  implicit def minSpanningTreeToGraph[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: base.GraphBound[V, Weighted[W], UndiEdge, G])(implicit cmp: Ordering[W]) =
    new MinSpanningTree[V, W, base.GraphBound[V, Weighted[W], UndiEdge, G]](g)

  // Kruskal
  implicit def graphToKruskal[V, W, G  <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: base.GraphBound[V, Weighted[W], UndiEdge, G])(implicit cmp: Ordering[W]) =
    new Kruskal[V, W, base.GraphBound[V, Weighted[W], UndiEdge, G]](g)    
 
  // Prim
  implicit def graphToPrim[V, W, G <: base.GraphBound[V, Weighted[W], UndiEdge, G]](g: base.GraphBound[V, Weighted[W], UndiEdge, G])(implicit cmp: Ordering[W]) =
    new Prim[V, W, base.GraphBound[V, Weighted[W], UndiEdge, G]](g)

  // AllPairsShortestPath
  implicit def graphToAllPairsShortestPath[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: base.GraphBound[V, Weighted[W], E, G])(implicit num: Numeric[W]) =
    new AllPairsShortestPath[V, W, E, base.GraphBound[V, Weighted[W], E, G]](g)
    
  // ShortestPath
  implicit def graphToShortestPath[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E], G <: base.GraphBound[V, Weighted[W], E, G]](g: base.GraphBound[V, Weighted[W], E, G])(implicit num: Numeric[W]) =
    new ShortestPath[V, W, E, base.GraphBound[V, Weighted[W], E, G]](g)
}
