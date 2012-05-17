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
sealed trait UnwEdgeLike[+V, +E] extends EdgeLike[V, E]

/** A template trait for the weighted edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait WEdgeLike[+V,+W, +UwE, +E] extends EdgeLike[V, E]
{
  def weight: W

  def toUnweightedEdge: UwE
}

/** A trait for conversion from the unweighted edge to the weighted edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait UnwEdgeToWEdge[+V, +WE[+_, +_]]
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
case class DiEdge[+V](in: V, out: V) extends DiEdgeLike[V, UndiEdge[V], DiEdge[V]] with UnwEdgeLike[V, DiEdge[V]] with UnwEdgeToWEdge[V, WDiEdge]
{
  override def _1: V =
    in

  override def _2: V =
    out

  override def swap: DiEdge[V] =
    DiEdge(out, in)

  override def toUndirectedEdge: UndiEdge[V] = 
    UndiEdge(in, out)

  override def % [W](weight: W): WDiEdge[V, W] =
    WDiEdge(in, out, weight)

  override def toString: String =
    in + " -> " + out
}

/** A class for the directed edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WDiEdge[+V, +W](in: V, out: V, weight: W) extends DiEdgeLike[V, WUndiEdge[V, W], WDiEdge[V, W]] with WEdgeLike[V, W, DiEdge[V], WDiEdge[V, W]]
{ 
  override def _1: V =
    in

  override def _2: V = 
    out
  
  override def swap: WDiEdge[V, W] =
    WDiEdge(out, in, weight)

  override def toUndirectedEdge: WUndiEdge[V, W] = 
    WUndiEdge(in, out, weight)
    
  override def toUnweightedEdge: DiEdge[V] =
    DiEdge(in, out)

  override def toString: String =
    in + " -> " + out + " % " + weight
}

/** A class for the undirected edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class UndiEdge[+V](_1: V, _2: V) extends UndiEdgeLike[V, DiEdge[V], UndiEdge[V]] with UnwEdgeLike[V, UndiEdge[V]] with UnwEdgeToWEdge[V, WUndiEdge]
{
  override def in: V =
    _1
    
  override def out: V =
    _2
  
  override def swap: UndiEdge[V] =
    UndiEdge(out, in)
  
  override def directedEdges: (DiEdge[V], DiEdge[V]) =
    (DiEdge(_1, _2), DiEdge(_2, _1))
    
  override def % [W](weight: W): WUndiEdge[V, W] =
    WUndiEdge(_1, _2, weight)
    
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
case class WUndiEdge[+V, +W](_1: V, _2: V, weight: W) extends UndiEdgeLike[V, WDiEdge[V, W], WUndiEdge[V, W]] with WEdgeLike[V, W, UndiEdge[V], WUndiEdge[V, W]]
{ 
  override def in: V =
    _1
    
  override def out: V =
    _2

  override def swap: WUndiEdge[V, W] =
    WUndiEdge(out, in, weight)

  override def directedEdges: (WDiEdge[V, W], WDiEdge[V, W]) =
    (WDiEdge(_1, _2, weight), WDiEdge(_2, _1, weight))

  override def toUnweightedEdge: UndiEdge[V] =
    UndiEdge(in, out)
  
  override def equals(that: Any) =
    that match {
      case WUndiEdge(v1, v2, w) => ((_1 == v1 && _2 == v2) || (_1 == v2 && _2 == v1)) && weight == w
      case _                           => false
    }

  override def toString: String =
    _1 + " ~ " + _2 + " % " + weight
}