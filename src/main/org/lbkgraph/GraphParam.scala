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
sealed trait EdgeLike[+V, +E] extends GraphParam[V, E] with Product2[V, V]
{  
  def vertices: (V, V) =
    (_1, _2)
 
  def in: V
  
  def out: V
  
  def swap: E

  def isDirected: Boolean
}

/** A template trait for the directed edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait DiEdgeLike[+V, +UndiE, +E] extends EdgeLike[V, E]
{
  def toUndirectedEdge: UndiE
 
  override def isDirected: Boolean =
    true
}

/** A template trait for the undirected edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait UndiEdgeLike[+V, +DiE, +E] extends EdgeLike[V, E]
{
  def directedEdges: (DiE, DiE)
  
  override def isDirected: Boolean =
    false
}

/** A template trait for the unweighted edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait UnweightedEdgeLike[+V, +E] extends EdgeLike[V, E]

/** A template trait for the weighted edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait WeightedEdgeLike[+V, +W, +UwE, +E] extends EdgeLike[V, E]
{
  def weight: W

  def toUnweightedEdge: UwE
}

/** A trait for conversion from the unweighted edge to the weighted edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait UnweightedEdgeToWeightedEdge[+V, +WE[+_, +_]]
{
  def % [W](weight: W): WE[V, W]
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
case class DiEdge[+V](in: V, out: V) extends DiEdgeLike[V, UndiEdge[V], DiEdge[V]] with UnweightedEdgeLike[V, DiEdge[V]] with UnweightedEdgeToWeightedEdge[V, WeightedDiEdge]
{
  override def _1: V =
    in

  override def _2: V =
    out

  override def swap: DiEdge[V] =
    DiEdge(out, in)

  override def toUndirectedEdge: UndiEdge[V] = 
    UndiEdge(in, out)

  override def % [W](weight: W): WeightedDiEdge[V, W] =
    WeightedDiEdge(in, out, weight)

  override def toString: String =
    in + " -> " + out
}

/** A class for the directed edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedDiEdge[+V, +W](in: V, out: V, weight: W) extends DiEdgeLike[V, WeightedUndiEdge[V, W], WeightedDiEdge[V, W]] with WeightedEdgeLike[V, W, DiEdge[V], WeightedDiEdge[V, W]]
{ 
  override def _1: V =
    in

  override def _2: V = 
    out
  
  override def swap: WeightedDiEdge[V, W] =
    WeightedDiEdge(out, in, weight)

  override def toUndirectedEdge: WeightedUndiEdge[V, W] = 
    WeightedUndiEdge(in, out, weight)
    
  override def toUnweightedEdge: DiEdge[V] =
    DiEdge(in, out)

  override def toString: String =
    in + " -> " + out + " % " + weight
}

/** A class for the undirected edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class UndiEdge[+V](_1: V, _2: V) extends UndiEdgeLike[V, DiEdge[V], UndiEdge[V]] with UnweightedEdgeLike[V, UndiEdge[V]] with UnweightedEdgeToWeightedEdge[V, WeightedUndiEdge]
{
  override def in: V =
    _1
    
  override def out: V =
    _2
  
  override def swap: UndiEdge[V] =
    UndiEdge(out, in)
  
  override def directedEdges: (DiEdge[V], DiEdge[V]) =
    (DiEdge(_1, _2), DiEdge(_2, _1))
    
  override def % [W](weight: W): WeightedUndiEdge[V, W] =
    WeightedUndiEdge(_1, _2, weight)
    
  override def equals(that: Any) =
    that match {
      case UndiEdge(v1, v2) => (_1 == v1 && _2 == v2) || (_1 == v2 && _2 == v1)
      case _                   => false
    }

  override def toString: String = _1 + " ~ " + _2
}

/** A class for the undirected edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WeightedUndiEdge[+V, +W](_1: V, _2: V, weight: W) extends UndiEdgeLike[V, WeightedDiEdge[V, W], WeightedUndiEdge[V, W]] with WeightedEdgeLike[V, W, UndiEdge[V], WeightedUndiEdge[V, W]]
{ 
  override def in: V =
    _1
    
  override def out: V =
    _2

  override def swap: WeightedUndiEdge[V, W] =
    WeightedUndiEdge(out, in, weight)

  override def directedEdges: (WeightedDiEdge[V, W], WeightedDiEdge[V, W]) =
    (WeightedDiEdge(_1, _2, weight), WeightedDiEdge(_2, _1, weight))

  override def toUnweightedEdge: UndiEdge[V] =
    UndiEdge(in, out)
  
  override def equals(that: Any) =
    that match {
      case WeightedUndiEdge(v1, v2, w) => ((_1 == v1 && _2 == v2) || (_1 == v2 && _2 == v1)) && weight == w
      case _                           => false
    }

  override def toString: String =
    _1 + " ~ " + _2 + " % " + weight
}