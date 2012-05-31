package org.lkbgraph.immutable
import scala.collection.mutable.Builder
import scala.collection.mutable.SetBuilder
import org.lkbgraph._

/** A factory class for immutable graph. 
 * 
 * @author Łukasz Szpakowski
 */
abstract class GraphFactory[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: base.GraphLike[XV, XX, XE, GG[XV, XX, XE]] with Graph[XV, XX, XE]] extends base.GraphFactory[GG] 
{
  override def newBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z,  E]]: Builder[GraphParam[V, X, E], GG[V, X, E]] =
    new SetBuilder(empty)
}