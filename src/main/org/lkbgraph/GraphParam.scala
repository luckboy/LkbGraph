package org.lkbgraph

/** A trait for graph parameter.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait GraphParam[+V, +X, +E[+_, +_]] extends Product

/** A template trait for the edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait EdgeLike[+V, +X, E[+_, +_]] extends GraphParam[V, X, E] with Product2[V, V]
{
  def vertices: (V, V) =
    (_1, _2)
 
  def in: V
  
  def out: V
  
  def swap: E[V, X]

  def isDirected: Boolean
  
  def hasWeight: Boolean
  
  def toUndirectedEdge: UndiEdge[V, X]
  
  def toUnweightedEdge: E[V, Unweighted]
  
  def ==~ [V1 >: V](e: E[V1, _]): Boolean
  
  def !=~ [V1 >: V](e: E[V1, _]): Boolean =
    !(this ==~ e)
}

/** A trait for the directed edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait DiEdge[+V, +X] extends EdgeLike[V, X, DiEdge]
{
  override def isDirected: Boolean =
    true
 
  override def ==~ [V1 >: V](e: DiEdge[V1, _]): Boolean =
    in == e.in && out == e.out    
}

/** A trait for the undirected edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait UndiEdge[+V, +X] extends EdgeLike[V, X, UndiEdge]
{
  override def isDirected: Boolean =
    false
    
  override def ==~ [V1 >: V](e: UndiEdge[V1, _]): Boolean =
    (_1 == e._1 && _2 == e._2) || (_1 == e._2 && _2 == e._1)

  def directedEdges: (DiEdge[V, X], DiEdge[V, X])

  override def toUndirectedEdge: UndiEdge[V, X] =
    this
}

/** A trait for weighted edge. */
sealed trait Weighted[+W]
{
  def weight: W
}

/** A trait for unweighted edge. */
sealed trait Unweighted

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
case class Vertex[+V](value: V) extends GraphParam[V, Nothing, Nothing]
{
  override def toString: String =
    "V(" + value + ")"
}

/** A class for the directed edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class UnwDiEdge[+V](in: V, out: V) extends DiEdge[V, Unweighted] with UnwEdgeToWEdge[V, WDiEdge]
{
  override def _1: V =
    in

  override def _2: V =
    out

  override def swap: UnwDiEdge[V] =
    UnwDiEdge(out, in)

  override def hasWeight: Boolean =
    false
  
  override def toUnweightedEdge: UnwDiEdge[V] =
    this

  override def toUndirectedEdge: UnwUndiEdge[V] =
    UnwUndiEdge(in, out)

  override def % [W](weight: W): WDiEdge[V, W] =
    WDiEdge(in, out, weight)

  override def toString: String =
    in + " -> " + out
}

/** A class for the directed edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WDiEdge[+V, +W](in: V, out: V, weight: W) extends DiEdge[V, Weighted[W]] with Weighted[W]
{ 
  override def _1: V =
    in

  override def _2: V = 
    out
  
  override def swap: WDiEdge[V, W] =
    WDiEdge(out, in, weight)

  override def hasWeight: Boolean =
    true

  override def toUnweightedEdge: UnwDiEdge[V] =
    UnwDiEdge(in, out)

  override def toUndirectedEdge: WUndiEdge[V, W] =
    WUndiEdge(in, out, weight)

  override def toString: String =
    in + " ~> " + out + " % " + weight
}

/** A class for the undirected edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class UnwUndiEdge[+V](_1: V, _2: V) extends UndiEdge[V, Unweighted] with UnwEdgeToWEdge[V, WUndiEdge]
{
  override def in: V =
    _1
    
  override def out: V =
    _2
  
  override def swap: UnwUndiEdge[V] =
    UnwUndiEdge(out, in)
  
  override def hasWeight: Boolean =
    false

  override def toUnweightedEdge: UnwUndiEdge[V] =
    this

  override def directedEdges: (UnwDiEdge[V], UnwDiEdge[V]) =
    (UnwDiEdge(_1, _2), UnwDiEdge(_1, _2))

  override def % [W](weight: W): WUndiEdge[V, W] =
    WUndiEdge(_1, _2, weight)
    
  override def equals(that: Any) =
    that match {
      case UnwUndiEdge(v1, v2) => (_1 == v1 && _2 == v2) || (_1 == v2 && _2 == v1)
      case _                   => false
    }

  override def hashCode: Int =
    _1.hashCode ^ _2.hashCode

  override def toString: String = 
    _1 + " ~ " + _2
}

/** A class for the undirected edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WUndiEdge[+V, +W](_1: V, _2: V, weight: W) extends UndiEdge[V, Weighted[W]]
{ 
  override def in: V =
    _1
    
  override def out: V =
    _2

  override def swap: WUndiEdge[V, W] =
    WUndiEdge(out, in, weight)

  override def hasWeight: Boolean =
    true

  override def directedEdges: (WDiEdge[V, W], WDiEdge[V, W]) =
    (WDiEdge(_1, _2, weight), WDiEdge(_1, _2, weight))
  
  override def toUnweightedEdge: UnwUndiEdge[V] =
    UnwUndiEdge(_1, _2)

  override def equals(that: Any) =
    that match {
      case WUndiEdge(v1, v2, w) => ((_1 == v1 && _2 == v2) || (_1 == v2 && _2 == v1)) && weight == w
      case _                    => false
    }
  
  override def hashCode: Int =
    _1.hashCode ^ _2.hashCode ^ weight.hashCode

  override def toString: String =
    _1 + " ~ " + _2 + " % " + weight
}