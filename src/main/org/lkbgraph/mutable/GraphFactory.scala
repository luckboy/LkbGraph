package org.lkbgraph.mutable
import scala.collection.mutable.GrowingBuilder
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A factory class for mutable graph. 
 * 
 * @author Łukasz Szpakowski
 */
abstract class GraphFactory[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: GraphLike[XV, XX, XE, GG[XV, XX, XE]] with Graph[XV, XX, XE]] extends base.GraphFactory[GG]
{
  override def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: Builder[GraphParam[V, X, E], GG[V, X, E]] =
    new GrowingBuilder[GraphParam[V, X, E], GG[V, X, E]](empty)
}