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
  
  def isDirected: Boolean

  def swap: E
}

/** A class for the vertex.
 * 
 * @author Łukasz Szpakowski
 */
case class Vertex[+V](value: V) extends GraphParam[V, Nothing]

/** A class for the directed edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class DiEdge[+V](in: V, out: V) extends EdgeLike[V, DiEdge[V]]
{
  override def swap: DiEdge[V] =
    DiEdge(out, in)
    
  override def isDirected: Boolean =
    true
}

/** A class for the directed edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedDiEdge[+V, +W](in: V, out: V, weight: W) extends EdgeLike[V, WeightedDiEdge[V, W]]
{ 
  override def swap: WeightedDiEdge[V, W] =
    WeightedDiEdge(out, in, weight)

  override def isDirected: Boolean =
    true
}

/** A class for the undirected edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class UndiEdge[+V](in: V, out: V) extends EdgeLike[V, UndiEdge[V]]
{
  override def swap: UndiEdge[V] =
    UndiEdge(out, in)
    
  override def isDirected: Boolean =
    false

  override def equals(that: Any) =
    that match {
      case UndiEdge(in2, out2) => (in == in2 && out == out2) || (in == out2 && out == in2)
      case _                   => false
    }
}

/** A class for the undirected edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedUndiEdge[+V, +W](in: V, out: V, weight: W) extends EdgeLike[V, WeightedUndiEdge[V, W]]
{ 
  override def swap: WeightedUndiEdge[V, W] =
    WeightedUndiEdge(out, in, weight)

  override def isDirected: Boolean =
    false 
    
  override def equals(that: Any) =
    that match {
      case WeightedUndiEdge(in2, out2, weight2) => 
        ((in == in2 && out == out2) || (in == out2 && out == in2)) && weight == weight2
      case _                                    =>
        false
    }
}