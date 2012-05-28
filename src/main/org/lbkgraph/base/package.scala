package org.lbkgraph

package object base
{
  type GraphBound[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E] , +G <: GraphLike[V, X, E, G] with Graph[V, X, E]] = GraphLike[V, X, E, G] with Graph[V, X, E]
}