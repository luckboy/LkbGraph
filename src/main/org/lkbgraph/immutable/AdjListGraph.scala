package org.lkbgraph.immutable
import org.lkbgraph._

/** A trait for the immutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
trait AdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.AdjListGraphLike[V, X, E, AdjListGraph[V, X, E]] with base.AdjListGraph[V, X, E] with Graph[V, X, E]
{
  override def empty: AdjListGraph[V, X, E] =
    AdjListGraph.empty
 
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    AdjListGraph.newBuilder
}

/** A singleton for the immutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
object AdjListGraph
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: AdjListGraph[V, X, E] = 
    new ImplAdjListGraph[V, X, E](Map())
  
  def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: collection.mutable.Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    new collection.mutable.SetBuilder[GraphParam[V, X, E], AdjListGraph[V, X, E]](empty)
    
  private class ImplAdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](protected val edgeLists: collection.Map[V, List[E[V, X]]]) extends AdjListGraph[V, X, E]
  {
    private lazy val params = (vertices.map { Vertex(_) } ++ edges)
    
    override protected def newAListGraph(es: collection.Map[V, List[E[V, X]]]): AdjListGraph[V, X, E] =
      new ImplAdjListGraph(es)
    
    override def iterator: Iterator[GraphParam[V, X, E]] =
      params.toIterator
  }
  
  /** Creates a new graph from the vertices and / or the edges.
   * @param params		the vertices and / or the edges.
   * @return			a new graph.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](params: GraphParam[V, X, E]*): Graph[V, X, E] =
    (newBuilder[V, X, E] ++= params).result
}