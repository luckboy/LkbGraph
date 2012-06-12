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
import scala.collection.GenTraversableOnce
import org.lkbgraph._

/** A template trait for mutable graph.
 * 
 * @author Łukasz Szpakowski
 */
trait GraphLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: GraphLike[V, X, E, G] with Graph[V, X, E]] extends base.GraphLike[V, X, E, G] with collection.mutable.SetLike[GraphParam[V, X, E], G]
{
  /** Adds the specified vertex to this graph.
   * @param v			the vertex.
   * @return			this graph.
   */
  def +@= (v: V): this.type
  
  /** Adds the specified edge to this graph.
   * @param e			the edge.
   * @return			this graph.
   */  
  def +~= (e: E[V, X]): this.type
  
  /** Removes the specified vertex from this graph.
   * @param v			the vertex.
   * @return			this graph.
   */
  def -@= (v: V): this.type
  
  /** Removes the specified edge from this graph.
   * @param e			the edge.
   * @return			this graph.
   */  
  def -~= (e: E[V, X]): this.type =
    -~!=(e.toUnweightedEdge)
  
  /** Removes the specified edge from this graph.
   * @param e			the edge.
   * @return			this graph.
   */
  def -~!= (e: E[V, Unweighted]): this.type  
  
  /** Removes the specified vertex and / or the specified edge from this graph.
   * @param e			the edge.
   * @return			this graph.
   */
  def -!= (param: GraphParam[V, Unweighted, E]): this.type =
    (param: @unchecked) match {
      case Vertex(v)           => -@=(v)
      case e: E[V, Unweighted] => -~!=(e)
    }
  
  /** Removes the specified vertices and / or the specified edges from this graph.
   * @param param1		the first vertex or the first edge.
   * @param param2		the second vertices or the second edges.
   * @param params		the vertices or the edges.
   * @return			this graph.
   */
  def -!= (param1: GraphParam[V, Unweighted, E], param2: GraphParam[V, Unweighted, E], params: GraphParam[V, Unweighted, E]*): this.type = {
    this -!= param1 -!= param2
    for(p <- params) this -!= p
    this
  }
  
  /** Removes the specified vertices and / or the specified edges from this graph.
   * @param params		the vertices or the edges.
   * @return			this graph.
   */
  def --!=(params: GenTraversableOnce[GraphParam[V, Unweighted, E]]): this.type = {
    for(p <- params) this -!= p
    this
  }
  
  override def += (param: GraphParam[V, X, E]): this.type =
    (param: @unchecked) match {
      case Vertex(v)  => +@=(v)
      case e: E[V, X] => +~=(e)
    }
  
  override def -= (param: GraphParam[V, X, E]): this.type =
    (param: @unchecked) match {
      case Vertex(v)  => -@=(v)
      case e: E[V, X] => -~=(e)
    }
}
