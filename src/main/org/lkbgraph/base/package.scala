package org.lkbgraph

package object base
{  
  type GraphBound[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E] , +G <: GraphLike[V, X, E, G] with Graph[V, X, E]] = GraphLike[V, X, E, G] with Graph[V, X, E]

  type UnwGraphBound[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E] , +G <: GraphLike[V, Unweighted, E, G] with Graph[V, Unweighted, E]] = GraphLike[V, Unweighted, E, G] with Graph[V, Unweighted, E]
  
  type WGraphBound[V, W, E[+Y, +Z] <: EdgeLike[Y, Z, E] , +G <: GraphLike[V, Weighted[W], E, G] with Graph[V, Weighted[W], E]] = GraphLike[V, Weighted[W], E, G] with Graph[V, Weighted[W], E]
}