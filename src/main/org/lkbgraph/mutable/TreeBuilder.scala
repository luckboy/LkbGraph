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
import scala.annotation.tailrec
import scala.collection.mutable.Builder
import org.lkbgraph._
import org.lkbgraph.immutable._

/** A builder class for tree.
 * 
 * @author Łukasz Szpakowski
 */
class TreeBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], T <: Tree[V, X, E] with TreeLike[V, X, E, _, T]](empty: T) extends Builder[E[V, X], T]
{
  private val mEdges = collection.mutable.Map[V, E[V, X]]()
  
  override def += (elem: E[V, X]): this.type = {
    mEdges += (elem.in -> elem)
    if(!elem.isDirected) (elem.out -> elem)
    this
  }
	
  override def clear(): Unit =
    mEdges.clear()  
    
  override def result(): T =
    (1 to mEdges.size).foldLeft(empty, mEdges.clone()) {
      case ((t, es), _) => 
        t.nodes.find { v => es.contains(v) }.flatMap { v => es.get(v).map { e => (t +~^ e, es -= v) } }.getOrElse(t, es)
    }._1
}
