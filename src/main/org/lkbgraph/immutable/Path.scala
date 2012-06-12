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

package org.lkbgraph.immutable
import scala.collection.mutable.Builder
import org.lkbgraph._

/** A trait for path.
 * 
 * @author Łukasz Szpakowski
 */
trait Path[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends PathLike[V, X, E, Graph[V, X, E], Tree[V, X, E], Path[V, X, E]] with Graph[V, X, E] with Tree[V, X, E]
{
  override def newPathBuilder(root: V): Builder[E[V, X], Path[V, X, E]] =
    Path.newPathBuilder(root)
}

/** A singleton for path.
 * 
 * @author Łukasz Szpakowski
 */
object Path extends PathFactory[Path]
{
  override def pathEmpty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): Path[V, X, E] =
    new ImplPath(root, Seq())
  
  private class ImplPath[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](val root: V, val edgeSeq: Seq[E[V, X]]) extends Path[V, X, E]
  {
    override def +~/ (e: E[V, X]): Path[V, X, E] = {
      val v = edgeSeq.lastOption.map { le => le.out }.getOrElse(root)
      val e2 = if(!e.isDirected && e.in != v) e.swap else e
      new ImplPath(root, edgeSeq :+ e)
    }
      
    override def -@/ (s: V): Path[V, X, E] =
      new ImplPath(root, edgeSeq.takeWhile { _.in != s })
  }
}
