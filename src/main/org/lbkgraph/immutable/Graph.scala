package org.lbkgraph.immutable
import scala.collection.immutable.ListMap
import org.lbkgraph._

/** A trait for the immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, E <: EdgeLike[V, E]] extends base.GraphLike[V, E, Graph[V, E]] with base.Graph[V, E]
{
  override def empty: Graph[V, E] =
    Graph.empty
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, E], Graph[V, E]] = 
    Graph.newBuilder
}

/** A singleton for the immutable graph.
 * 
 * @author Łukasz Szpakowski
 */
object Graph
{
  def empty[V, E <: EdgeLike[V, E]]: Graph[V, E] = 
    new ImplGraph[V, E](Map())
  
  def newBuilder[V, E <: EdgeLike[V, E]]: collection.mutable.Builder[GraphParam[V, E], Graph[V, E]] =
    new collection.mutable.SetBuilder(empty)
  
  private class ImplGraph[V, E <: EdgeLike[V, E]](val mEdgeMaps: Map[V, Map[GenUnwEdge[V], E]]) extends Graph[V, E]
  {
    lazy val mParams: Set[GraphParam[V, E]] = vertexSet.map { Vertex(_) } ++ edgeSet
    
    override def vertices: Iterable[V] =
      mEdgeMaps.keys
      
    override def edges: Iterable[E] =
      mEdgeMaps.flatMap { _._2.map { _._2 } }.toSet
      
    private def edgeMapFrom(s: V) =
      mEdgeMaps.getOrElse(s, ListMap[GenUnwEdge[V], E]())
      
    override def edgesFrom(s: V): Iterable[E] =
      edgeMapFrom(s).values
      
    override def contains(param: GraphParam[V, E]): Boolean =
      (param: @unchecked) match {
        case Vertex(v) => mEdgeMaps.keys.exists(v ==) 
        case e: E      => edgesFrom(e.in).exists(e ==)
      }
    
    override def iterator: Iterator[GraphParam[V, E]] =
      mParams.toIterator
    
    override def + (param: GraphParam[V, E]): Graph[V, E] =
      (param: @unchecked) match {
        case Vertex(v) => 
          new ImplGraph(mEdgeMaps + (v -> ListMap[GenUnwEdge[V], E]()))
        case e: E      =>
          if(e._1 != e._2) {
            val newEdgeMaps1 = mEdgeMaps + (e.in -> (edgeMapFrom(e.in) + (e.toUnweightedEdge -> e)))
            val newEdgeMaps2 = if(e.isDirected)
                newEdgeMaps1 + (e.out -> edgeMapFrom(e.out))
              else
                newEdgeMaps1 + (e.out -> (edgeMapFrom(e.out) + (e.swap.toUnweightedEdge -> e.swap)))
            new ImplGraph(newEdgeMaps2)
          } else
            this
      }
    
    override def - (param: GraphParam[V, E]): Graph[V, E] =
      (param: @unchecked) match {
        case Vertex(v) =>
          new ImplGraph(mEdgeMaps.map { case (u, em) => (u -> em.filter { _._1.out == v }) } - v)
        case e: E      =>
          if(e._1 != e._2) {
            val newEdgeMaps1 = mEdgeMaps + (e.in -> (edgeMapFrom(e.in).filterNot { _._2 == e }))
            val newEdgeMaps2 = if(e.isDirected)
               	newEdgeMaps1
              else
                newEdgeMaps1 + (e.out -> (edgeMapFrom(e.out).filterNot { _._2 == e }))
            new ImplGraph(newEdgeMaps2)
          } else
            this
      }
    
    override def stringPrefix: String =
      "Graph"
  }
  
  /** Creates a new graph from the vertices and / or the edges.
   * @param params		the vertices and / or the edges.
   * @return			a new graph.
   */
  def apply[V, E <: EdgeLike[V, E]](params: GraphParam[V, E]*): Graph[V, E] =
    (newBuilder[V, E] ++= params).result
}