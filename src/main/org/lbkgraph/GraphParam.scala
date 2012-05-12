package org.lbkgraph

/** A trait for graph parameter.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait GraphParam[+V, +E]

/** A template trait for the edge.
 * 
 * @author Łukasz Szpakowski
 */
trait EdgeLike[+V, +E] extends GraphParam[V, E] with Product2[V, V]
{
  def in: V
    
  def out: V
 
  override def _1: V = in
  
  override def _2: V = out  

  def swap: E
}

/** A class for the vertex.
 * 
 * @author Łukasz Szpakowski
 */
case class Vertex[+V](value: V) extends GraphParam[V, Nothing]

/** A class for the edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class Edge[+V](in: V, out: V) extends EdgeLike[V, Edge[V]]
{
  override def swap: Edge[V] =
    Edge(out, in)
}

/** A class for the weighted edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedEdge[+V, +W](in: V, out: V, weight: W) extends EdgeLike[V, WeightedEdge[V, W]]
{ 
  override def swap: WeightedEdge[V, W] =
    WeightedEdge(out, in, weight)
}