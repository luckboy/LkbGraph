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
import org.lkbgraph._

/** A trait for the mutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
trait AdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends base.AdjListGraphLike[V, X, E, AdjListGraph[V, X, E]] with base.AdjListGraph[V, X, E] with GraphLike[V, X, E, AdjListGraph[V, X, E]] with Graph[V, X, E]
{
  override def empty: AdjListGraph[V, X, E] =
    AdjListGraph.empty
    
  override protected[this] def newBuilder: collection.mutable.Builder[GraphParam[V, X, E], AdjListGraph[V, X, E]] =
    AdjListGraph.newBuilder

    override protected def edgeLists: collection.mutable.Map[V, List[E[V, X]]]
  
  override def +@= (v: V): this.type = {
    edgeLists += (v -> edgeListFrom(v))
    this
  }
  
  override def +~= (e: E[V, X]): this.type = {
    if(e._1 != e._2) {
      edgeLists += (e.in -> edgeListFromWithEdge(e.in, e)) 
      edgeLists += (e.out -> (if(e.isDirected) edgeListFrom(e.out) else edgeListFromWithEdge(e.out, e.swap)))
    }
    this
  }
  
  override def -@= (v: V): this.type = {
    for((u, es) <- edgeLists.toMap) { edgeLists += (u -> es.filterNot { _.out == v }) }
    edgeLists -= v
    this
  }
  
  override def -~!= (e: E[V, Unweighted]): this.type = {
    if(e._1 != e._2) {
      if(edgeLists.contains(e.in)) edgeLists += (e.in -> edgeListFrom(e.in).filterNot { _ ==~ e })
      if(!e.isDirected && edgeLists.contains(e.out)) edgeLists += (e.out -> edgeListFrom(e.out).filterNot { _ ==~ e })
    }
    this
  }
}

/** A singleton for the mutable version of the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
object AdjListGraph extends GraphFactory[AdjListGraph]
{
  def empty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]: AdjListGraph[V, X, E] = 
    new ImplAdjListGraph[V, X, E](collection.mutable.Map())
      
  private class ImplAdjListGraph[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](protected val edgeLists: collection.mutable.Map[V, List[E[V, X]]]) extends AdjListGraph[V, X, E]
  {
    override protected def newAdjListGraph(es: collection.Map[V, List[E[V, X]]]): AdjListGraph[V, X, E] =
      es match {
        case mes: collection.mutable.Map[V, List[E[V, X]]] => new ImplAdjListGraph(mes)
        case _                                             => new ImplAdjListGraph(collection.mutable.Map() ++ es)
      }
  }  
}