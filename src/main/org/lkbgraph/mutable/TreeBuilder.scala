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
  private val mEdgeLists = collection.mutable.Map[V, List[E[V, X]]]()
  
  override def += (elem: E[V, X]): this.type = {
    mEdgeLists += (elem.in -> (elem :: mEdgeLists.getOrElse(elem.in, Nil)))
    if(!elem.isDirected) mEdgeLists += (elem.out -> (elem :: mEdgeLists.getOrElse(elem.out, Nil)))
    this
  }
	
  override def clear(): Unit =
    mEdgeLists.clear()  
    
  override def result(): T =
    (1 to mEdgeLists.size).foldLeft(empty, mEdgeLists.clone()) {
      case ((t, els), _) => 
        t.nodes.find(els.contains).flatMap { v => els.get(v).map { es => (es.foldLeft(t){ _ +~^ _ }, els -= v) } }.getOrElse(t, els)
    }._1
}
