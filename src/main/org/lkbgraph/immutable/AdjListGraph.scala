package org.lkbgraph.immutable
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for the immutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
trait AdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.AdjListGraphLike[V, X, E, AdjListGraph[V, X, E]] with base.AdjListGraph[V, X, E] with Graph[V, X, E]
{
  override def empty: AdjListGraph[V, X, E] =
    AdjListGraph.empty
 
  override protected[this] def newBuilder: Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    AdjListGraph.newBuilder
}

/** A singleton for the immutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
object AdjListGraph extends GraphFactory[AdjListGraph]
{
  implicit def canBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[AdjListGraph[_, _, E1], GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    graphCanBuildFrom
  
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: AdjListGraph[V, X, E] = 
    new ImplAdjListGraph[V, X, E](Map())
      
  private class ImplAdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](protected val edgeLists: collection.Map[V, List[E[V, X]]]) extends AdjListGraph[V, X, E]
  {
    private lazy val params = (vertices.map { Vertex(_) } ++ edges)
    
    override protected def newAdjListGraph(es: collection.Map[V, List[E[V, X]]]): AdjListGraph[V, X, E] =
      new ImplAdjListGraph(es)
    
    override def iterator: Iterator[GraphParam[V, X, E]] =
      params.toIterator
  }  
}