package org.lbkgraph

/** A trait for graph parameter.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait GraphParam[+V, +E]

/** A trait for the edge that for the internal implementation of the graph.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait GenEdgeLike[+V, +E] extends GraphParam[V, E] with Product2[V, V]
{
  def vertices: (V, V) =
    (_1, _2)
 
  def in: V
  
  def out: V
  
  def swap: E

  def isDirected: Boolean
  
  def isWeighted: Boolean
  
  def toUndirectedEdge: GenUndiEdge[V]
  
  def toUnweightedEdge: GenUnwEdge[V]
}

/** A trait for the undirected edge that for the internal implementation of the graph.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait GenUndiEdge[+V] extends GenEdgeLike[V, GenUndiEdge[V]]
{
  override def isDirected: Boolean =
    false
}

/** A trait for the unweighted edge that for the internal implementation of the graph.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait GenUnwEdge[+V] extends GenEdgeLike[V, GenUnwEdge[V]]
{
  override def isWeighted: Boolean =
    false
}

/** A template trait for the edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait EdgeLike[+V, +E] extends GenEdgeLike[V, E]

/** A template trait for the directed edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait DiEdgeLike[+V, +UndiE <: GenUndiEdge[V], +E] extends EdgeLike[V, E]
{
  def toUndirectedEdge: UndiE
 
  override def isDirected: Boolean =
    true
}

/** A template trait for the undirected edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait UndiEdgeLike[+V, +DiE, +E <: GenUndiEdge[V]] extends EdgeLike[V, E] with GenUndiEdge[V]
{
  def directedEdges: (DiE, DiE)
  
  override def toUndirectedEdge: UndiEdgeLike[V, DiE, E] =
    this

  override def isDirected: Boolean =
    false
}

/** A template trait for the unweighted edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait UnwEdgeLike[+V, +E <: GenUnwEdge[V]] extends EdgeLike[V, E] with GenUnwEdge[V]
{
  override def toUnweightedEdge: UnwEdgeLike[V, E] =
    this
}

/** A template trait for the weighted edge.
 * 
 * @author Łukasz Szpakowski
 */
sealed trait WEdgeLike[+V,+W, +UnwE <: GenUnwEdge[V], +E] extends EdgeLike[V, E]
{
  def weight: W

  override def isWeighted: Boolean =
    true

  override def toUnweightedEdge: UnwE
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
case class UnwDiEdge[+V](in: V, out: V) extends DiEdgeLike[V, UnwUndiEdge[V], UnwDiEdge[V]] with UnwEdgeLike[V, UnwDiEdge[V]] with UnwEdgeToWEdge[V, WDiEdge]
{
  override def _1: V =
    in

  override def _2: V =
    out

  override def swap: UnwDiEdge[V] =
    UnwDiEdge(out, in)

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
case class WDiEdge[+V, +W](in: V, out: V, weight: W) extends DiEdgeLike[V, WUndiEdge[V, W], WDiEdge[V, W]] with WEdgeLike[V, W, UnwDiEdge[V], WDiEdge[V, W]]
{ 
  override def _1: V =
    in

  override def _2: V = 
    out
  
  override def swap: WDiEdge[V, W] =
    WDiEdge(out, in, weight)

  override def toUndirectedEdge: WUndiEdge[V, W] = 
    WUndiEdge(in, out, weight)
    
  override def toUnweightedEdge: UnwDiEdge[V] =
    UnwDiEdge(in, out)

  override def toString: String =
    in + " -> " + out + " % " + weight
}

/** A class for the undirected edge without weight.
 * 
 * @author Łukasz Szpakowski
 */
case class UnwUndiEdge[+V](_1: V, _2: V) extends UndiEdgeLike[V, UnwDiEdge[V], UnwUndiEdge[V]] with UnwEdgeLike[V, UnwUndiEdge[V]] with UnwEdgeToWEdge[V, WUndiEdge]
{
  override def in: V =
    _1
    
  override def out: V =
    _2
  
  override def swap: UnwUndiEdge[V] =
    UnwUndiEdge(out, in)
  
  override def directedEdges: (UnwDiEdge[V], UnwDiEdge[V]) =
    (UnwDiEdge(_1, _2), UnwDiEdge(_2, _1))
    
  override def % [W](weight: W): WUndiEdge[V, W] =
    WUndiEdge(_1, _2, weight)
    
  override def equals(that: Any) =
    that match {
      case UnwUndiEdge(v1, v2) => (_1 == v1 && _2 == v2) || (_1 == v2 && _2 == v1)
      case _                   => false
    }

  override def toString: String = _1 + " ~ " + _2
}

/** A class for the undirected edge that has weight.
 * 
 * @author Łukasz Szpakowski
 */
case class WUndiEdge[+V, +W](_1: V, _2: V, weight: W) extends UndiEdgeLike[V, WDiEdge[V, W], WUndiEdge[V, W]] with WEdgeLike[V, W, UnwUndiEdge[V], WUndiEdge[V, W]]
{ 
  override def in: V =
    _1
    
  override def out: V =
    _2

  override def swap: WUndiEdge[V, W] =
    WUndiEdge(out, in, weight)

  override def directedEdges: (WDiEdge[V, W], WDiEdge[V, W]) =
    (WDiEdge(_1, _2, weight), WDiEdge(_2, _1, weight))

  override def toUnweightedEdge: UnwUndiEdge[V] =
    UnwUndiEdge(in, out)
  
  override def equals(that: Any) =
    that match {
      case WUndiEdge(v1, v2, w) => ((_1 == v1 && _2 == v2) || (_1 == v2 && _2 == v1)) && weight == w
      case _                    => false
    }

  override def toString: String =
    _1 + " ~ " + _2 + " % " + weight
}