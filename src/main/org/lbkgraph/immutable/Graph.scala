package org.lbkgraph.immutable
import scala.collection.immutable.ListMap
import org.lbkgraph._

/** A trait for the immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.GraphLike[V, X, E, Graph[V, X, E]] with base.Graph[V, X, E]
{
  override def empty: Graph[V, X, E] = throw new Exception
    Graph.empty
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], Graph[V, X, E]] = throw new Exception
    Graph.newBuilder
}

/** A singleton for the immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
object Graph
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: Graph[V, X, E] = 
    new ImplGraph[V, X, E](Map())
  
  def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: collection.mutable.Builder[GraphParam[V, X, E], Graph[V, X, E]] =
    new collection.mutable.SetBuilder[GraphParam[V, X, E], Graph[V, X, E]](empty)
  
  private class ImplGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](val mEdgeMaps: Map[V, Map[E[V, Unweighted], E[V, X]]]) extends Graph[V, X, E]
  {
    lazy val mParams: Set[GraphParam[V, X, E]] = vertexSet.map { Vertex(_) } ++ edgeSet
    
    override def vertices: Iterable[V] =
      mEdgeMaps.keys
      
    override def edges: Iterable[E[V, X]] =
      mEdgeMaps.flatMap { _._2.map { _._2 } }.toSet
      
    private def edgeMapFrom(s: V) =
      mEdgeMaps.getOrElse(s, ListMap[E[V, Unweighted], E[V, X]]())
      
    override def edgesFrom(s: V): Iterable[E[V, X]] =
      edgeMapFrom(s).values

    override def iterator: Iterator[GraphParam[V, X, E]] =
      mParams.toIterator

    override def containsVertex(v: V): Boolean =
      mEdgeMaps.keys.exists(v ==)
      
    override def containsEdge(e: E[V, X]): Boolean =
      edgesFrom(e.in).exists(e ==)
          
    override def +/ (v: V): Graph[V, X, E] =
      new ImplGraph(mEdgeMaps + (v -> ListMap[E[V, Unweighted], E[V, X]]()))
      
    override def +~ (e: E[V, X]): Graph[V, X, E] =
      if(e._1 != e._2) {
        val newEdgeMaps1 = mEdgeMaps + (e.in -> (edgeMapFrom(e.in) + (e.toUnweightedEdge -> e)))
        val newEdgeMaps2 = if(e.isDirected)
            newEdgeMaps1 + (e.out -> edgeMapFrom(e.out))
          else
            newEdgeMaps1 + (e.out -> (edgeMapFrom(e.out) + (e.swap.toUnweightedEdge -> e.swap)))
        new ImplGraph(newEdgeMaps2)
      } else
        this
    
    override def -/ (v: V): Graph[V, X, E] =
      new ImplGraph(mEdgeMaps.map { case (u, em) => (u -> em.filter { _._1.out == v }) } - v)      
      
    override def -~! (e: E[V, Unweighted]): Graph[V, X, E] =
      if(e._1 != e._2) {
        val newEdgeMaps1 = mEdgeMaps + (e.in -> (edgeMapFrom(e.in) - e))
        val newEdgeMaps2 = if(e.isDirected)
            newEdgeMaps1
          else
            newEdgeMaps1 + (e.out -> (edgeMapFrom(e.out) - e))
        new ImplGraph(newEdgeMaps2)
      } else
        this
      
    override def stringPrefix: String =
      "Graph"
  }
  
  /** Creates a new graph from the vertices and / or the edges.
   * @param params		the vertices and / or the edges.
   * @return			a new graph.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](params: GraphParam[V, X, E]*): Graph[V, X, E] =
    (newBuilder[V, X, E] ++= params).result
}