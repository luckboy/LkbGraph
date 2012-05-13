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
  
  def vertices: (V, V) = (in, out)
  
  def isDirected: Boolean

  def swap: E
}

/** A template trait for the directed edge.
 * 
 * @author Łukasz Szpakowski
 */
trait DiEdgeLike[+V, +E] extends EdgeLike[V, E]
{
  override def isDirected: Boolean = true
}

/** A template trait for the undirected edge.
 * 
 * @author Łukasz Szpakowski
 */
trait UndiEdgeLike[+V, +E] extends EdgeLike[V, E]
{
  override def isDirected: Boolean = false
}

/** A template trait for the unweighted edge.
 * 
 * @author Łukasz Szpakowski
 */
trait UnweightedEdgeLike[+V, +E] extends EdgeLike[V, E]

/** A template trait for the weighted edge.
 * 
 * @author Łukasz Szpakowski
 */
trait WeightedEdgeLike[+V, +W, +UE, +E] extends EdgeLike[V, E]
{
  def weight: W
  
  def unweightedEdge: UE
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
case class DiEdge[+V](in: V, out: V) extends DiEdgeLike[V, DiEdge[V]] with UnweightedEdgeLike[V, DiEdge[V]]
{
  override def swap: DiEdge[V] = DiEdge(out, in)
  
  override def toString: String = in + " --> " + out
}

/** A class for the directed edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedDiEdge[+V, +W](in: V, out: V, weight: W) extends DiEdgeLike[V, WeightedDiEdge[V, W]] with WeightedEdgeLike[V, W, DiEdge[V], WeightedDiEdge[V, W]]
{ 
  override def swap: WeightedDiEdge[V, W] = WeightedDiEdge(out, in, weight)

  override def unweightedEdge: DiEdge[V] = DiEdge(in, out)

  override def toString: String = in + " -~ " + weight +" -> " + out
}

/** A class for the undirected edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class UndiEdge[+V](in: V, out: V) extends UndiEdgeLike[V, UndiEdge[V]] with UnweightedEdgeLike[V, UndiEdge[V]]
{
  override def swap: UndiEdge[V] = UndiEdge(out, in)
    
  override def equals(that: Any) =
    that match {
      case UndiEdge(in2, out2) => (in == in2 && out == out2) || (in == out2 && out == in2)
      case _                   => false
    }

  override def toString: String = in + " --- " + out
}

/** A class for the undirected edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedUndiEdge[+V, +W](in: V, out: V, weight: W) extends UndiEdgeLike[V, WeightedUndiEdge[V, W]] with WeightedEdgeLike[V, W, UndiEdge[V], WeightedUndiEdge[V, W]]
{ 
  override def swap: WeightedUndiEdge[V, W] = WeightedUndiEdge(out, in, weight)

  override def unweightedEdge: UndiEdge[V] = UndiEdge(in, out)
  
  override def equals(that: Any) =
    that match {
      case WeightedUndiEdge(in2, out2, weight2) => 
        ((in == in2 && out == out2) || (in == out2 && out == in2)) && weight == weight2
      case _                                    =>
        false
    }

  override def toString: String = in + " -~ " + weight +" -- " + out
}