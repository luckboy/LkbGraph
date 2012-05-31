package org.lkbgraph.mutable
import org.lkbgraph._

/** A trait for the mutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
trait AdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.AdjListGraphLike[V, X, E, AdjListGraph[V, X, E]] with base.AdjListGraph[V, X, E] with GraphLike[V, X, E, AdjListGraph[V, X, E]] with Graph[V, X, E]
{
  override def empty: AdjListGraph[V, X, E] =
    AdjListGraph.empty
    
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    AdjListGraph.newBuilder

    override protected def edgeLists: collection.mutable.Map[V, List[E[V, X]]]
  
  override def +@= (v: V): this.type = {
    edgeLists += (v -> Nil)
    this
  }
  
  override def +~= (e: E[V, X]): this.type = {
    if(e._1 != e._2) {
      edgeLists += (e.in -> edgeListFromWithEdge(e.in, e))
      if(!e.isDirected) edgeLists += (e.out -> edgeListFromWithEdge(e.out, e))
    }
    this
  }
  
  override def -@= (v: V): this.type = {
    for((u, es) <- edgeLists.toMap) { edgeLists += (u -> es.filterNot { _.out == v }) }
    edgeLists -= v
    this
  }
  
  override def -~!= (e: E[V, Unweighted]): this.type = {
    if(e._1 != e._2) {
      edgeLists += (e.in -> edgeListFrom(e.in).filterNot { _ ==~ e })
      if(!e.isDirected) edgeLists += (e.out -> edgeListFrom(e.out).filterNot { _ ==~ e })
    }
    this
  }
}

/** A singleton for the mutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
object AdjListGraph
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: AdjListGraph[V, X, E] = 
    new ImplAdjListGraph[V, X, E](collection.mutable.Map())
  
  def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: collection.mutable.Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    new collection.mutable.SetBuilder[GraphParam[V, X, E], AdjListGraph[V, X, E]](empty)
    
  private class ImplAdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](protected val edgeLists: collection.mutable.Map[V, List[E[V, X]]]) extends AdjListGraph[V, X, E]
  {
    override protected def newAListGraph(es: collection.Map[V, List[E[V, X]]]): AdjListGraph[V, X, E] =
      es match {
        case mes: collection.mutable.Map[V, List[E[V, X]]] => new ImplAdjListGraph(mes)
        case _                                             => new ImplAdjListGraph(collection.mutable.Map() ++ es)
      }
  }
  
  /** Creates a new graph from the vertices and / or the edges.
   * @param params		the vertices and / or the edges.
   * @return			a new graph.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](params: GraphParam[V, X, E]*): Graph[V, X, E] =
    (newBuilder[V, X, E] ++= params).result
}