/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.lkbgraph.mutable
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for the mutable version of the implementation of the incidence set representation.
 * 
 * @author Łukasz Szpakowski
 */
trait IncSetGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.IncSetGraphLike[V, X, E, IncSetGraph[V, X, E]] with base.IncSetGraph[V, X, E] with GraphLike[V, X, E, IncSetGraph[V, X, E]] with Graph[V, X, E]
{
  override def empty: IncSetGraph[V, X, E] =
    IncSetGraph.empty
    
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], IncSetGraph[V, X, E]] =
    IncSetGraph.newBuilder

  protected def privVertexSet: collection.mutable.Set[V]
  
  protected def edgeMap: collection.mutable.Map[E[V, Unweighted], E[V, X]]
  
  override def +@= (v: V): this.type = {
    privVertexSet += v
    this
  }
  
  override def +~= (e: E[V, X]): this.type = {
    privVertexSet += e.in += e.out
    edgeMap += (e.toUnweightedEdge -> e)
    this
  }
  
  override def -@= (v: V): this.type = {
    privVertexSet -= v
    edgeMap --= edgeMap.keys.filter { e => e.in == v || e.out == v }
    this
  }
  
  override def -~!= (e: E[V, Unweighted]): this.type = {
    edgeMap -= e
    this
  }
}

/** A singleton for the mutable version of the implementation of the incidence set representation.
 * 
 * @author Łukasz Szpakowski
 */
object IncSetGraph extends GraphFactory[IncSetGraph]
{
  implicit def canBuildFrom[E1[+Y1, +Z1] <: EdgeLike[Y1, Z1, E1], V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: CanBuildFrom[IncSetGraph[_, _, E1], GraphParam[V, X, E], IncSetGraph[V, X, E]] =
    graphCanBuildFrom

  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: IncSetGraph[V, X, E] = 
    new ImplIncSetGraph[V, X, E](collection.mutable.Set(), collection.mutable.Map())
      
  private class ImplIncSetGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](protected val privVertexSet: collection.mutable.Set[V], protected val edgeMap: collection.mutable.Map[E[V, Unweighted], E[V, X]]) extends IncSetGraph[V, X, E]
  {
    override protected def newIncSetGraph(vs: collection.Set[V], es: collection.Map[E[V, Unweighted], E[V, X]]): IncSetGraph[V, X, E] =
      (vs, es) match {
        case (mvs: collection.mutable.Set[V], mes: collection.mutable.Map[E[V, Unweighted], E[V, X]]) => new ImplIncSetGraph(mvs, mes)
        case _                                                                                        => new ImplIncSetGraph(collection.mutable.Set() ++ vs, collection.mutable.Map() ++ es)
      }
  }  
}
