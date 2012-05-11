package org.lbkgraph.mutable
import org.lbkgraph._

/** A trait for mutable graph. 
 * 
 * @author Łukasz Szpakowski
 */
trait Graph[V, E <: EdgeLike[V, E]] extends base.GraphLike[V, E, Graph[V, E]] with base.Graph[V, E] 
{
  override def empty: Graph[V, E] = throw new Exception
  
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, E], Graph[V, E]] = throw new Exception
}