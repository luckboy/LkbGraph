package org.lbkgraph

/** A trait for graph parameter.
 * 
 * @author Łukasz Szpakowski
 */
trait GraphParam[+V, +E]

/** A template trait for the edge.
 * 
 * @author Łukasz Szpakowski
 */
trait EdgeLike[+V, +E] extends GraphParam[V, E] with Product2[V, V]
{
  def source: V = _1
    
  def dest: V = _2
  
  def swap: E
}

/** A class for the vertice.
 * 
 * @author Łukasz Szpakowski
 */
case class Vertice[+V](value: V) extends GraphParam[V, Nothing]

/** A class for the edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class Edge[+V](_1: V, _2: V) extends EdgeLike[V, Edge[V]]
{
  override def swap: Edge[V] =
    Edge(_2, _1)
}

/** A class for the weighted edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedEdge[+V, +W](_1: V, _2: V, weight: W) extends EdgeLike[V, WeightedEdge[V, W]]
{
  override def swap: WeightedEdge[V, W] =
    WeightedEdge(_2, _1, weight)
}