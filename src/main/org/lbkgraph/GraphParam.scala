package org.lbkgraph

trait GraphParam[+V, +E]

trait EdgeLike[+V, +E] extends GraphParam[V, E] with Product2[V, V]
{
  def swap: E
}

case class Vertice[+V](value: V) extends GraphParam[V, Nothing]

case class Edge[+V](_1: V, _2: V) extends EdgeLike[V, Edge[V]]
{
  override def swap: Edge[V] =
    Edge(_2, _1)
}

case class WeightedEdge[+V, +W](_1: V, _2: V, weight: W) extends EdgeLike[V, WeightedEdge[V, W]]
{
  override def swap: WeightedEdge[V, W] =
    WeightedEdge(_2, _1, weight)
}