package org.lkbgraph.base
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.mutable.SetBuilder
import org.lkbgraph._

/** A factory class for graph. 
 * 
 * @author Łukasz Szpakowski
 */
abstract class GraphFactory[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: Graph[XV, XX, XE]]
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: GG[V, X, E]

  def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: Builder[GraphParam[V, X, E], GG[V, X, E]]
  
  /** Creates a new graph from the vertices and / or the edges.
   * @param params		the vertices and / or the edges.
   * @return			a new graph.
   */
  def apply[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](params: GraphParam[V, X, E]*): GG[V, X, E] =
    (newBuilder ++= params).result
    
  def graphCanBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[GG[_, _, E1], GraphParam[V, X, E], GG[V, X, E]] =
    new CanBuildFrom[GG[_, _, E1], GraphParam[V, X, E], GG[V, X, E]] {
      override def apply(from: GG[_, _, E1]): Builder[GraphParam[V, X, E], GG[V, X, E]] =
        newBuilder
        
      override def apply(): Builder[GraphParam[V, X, E], GG[V, X, E]] =
        newBuilder
    }  
}