package org.lbkgraph

package object base
{
  type GraphBound[V, E <: EdgeLike[V, E] , +G <: GraphLike[V, E, G] with Graph[V, E]] = GraphLike[V, E, G] with Graph[V, E]
}