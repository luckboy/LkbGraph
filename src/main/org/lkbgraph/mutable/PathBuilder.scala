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
import scala.collection.mutable.Builder
import org.lkbgraph._
import org.lkbgraph.immutable._

/** A builder class for tree.
 * 
 * @author Łukasz Szpakowski
 */
class PathBuilder[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], P <: Path[V, X, E] with PathLike[V, X, E, _, _, P]](empty: P) extends Builder[E[V, X], P]
{
  private var mEdges = collection.mutable.Map[V, E[V, X]]()
  
  override def += (elem: E[V, X]): this.type = {
    mEdges += (elem.in -> elem)
    if(!elem.isDirected) (elem.out -> elem)
    this
  }
	
  override def clear(): Unit =
    mEdges.clear()  

  override def result(): P =
    (1 to mEdges.size).foldLeft(empty, mEdges.clone()) {
      case ((p, es), _) => p.nodeSeq.lastOption.flatMap { v => es.get(v).map { e => (p +~/ e, es -= v) } }.getOrElse(p, es)
    }._1
}
