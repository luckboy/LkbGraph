package org.lkbgraph.base
import org.lkbgraph._

/** A template trait for the graph representation of the adjacency list.
 * 
 * @author Łukasz Szpakowski
 */
trait AdjListGraphLike[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E], +G <: AdjListGraphLike[V, X, E, G] with Graph[V, X, E]] extends GraphLike[V, X, E, G] 
{  
  protected def edgeLists: collection.Map[V, List[E[V, X]]]

  protected def newAListGraph(es: collection.Map[V, List[E[V, X]]]): G
  
  override def vertices: Iterable[V] =
    edgeLists.keys
    
  override def edges: Iterable[E[V, X]] =
    edgesIterator.toIterable
    
  override def edgesIterator: Iterator[E[V, X]] =
    edgeLists.iterator.foldLeft(Iterator.empty: Iterator[E[V, X]], Set[V]()) { 
      case ((iter, vs), (v, es)) => (iter ++ es.iterator.filterNot { e => vs.contains(e.out) }, vs + v)
    }._1
    
  protected def edgeListFrom(s: V) =
    edgeLists.getOrElse(s, Nil)
    
  override def edgesFrom(s: V): Iterable[E[V, X]] =
    edgeListFrom(s)
    
  override def containsVertex(v: V): Boolean =
    edgeLists.contains(v)
    
  override def containsEdge(e: E[V, X]): Boolean =
    edgeLists.exists { _._2.contains(e) }
  
  override def +@ (v: V): G =
    newAListGraph(edgeLists + (v -> Nil))
    
  protected def edgeListFromWithEdge(s: V, e: E[V, X]) = {
    val es = edgeListFrom(s)
    if(es.exists { _ ==~ e }) es else (e :: es)
  }
  
  override def +~ (e: E[V, X]): G =
    if(e._1 != e._2) {
      val newEdgeLists1 = edgeLists + (e.in -> edgeListFromWithEdge(e.in, e))
      val newEdgeLists2 = if(e.isDirected)
          newEdgeLists1 + (e.out -> edgeListFrom(e.out))
        else
          newEdgeLists1 + (e.out -> edgeListFromWithEdge(e.out, e.swap))
      newAListGraph(newEdgeLists2)
    } else
      repr
      
  override def -@ (v: V): G =
    newAListGraph(edgeLists.map { case (u, es) => (u, es.filterNot { _.out == v }) } - v)
    
  override def -~! (e: E[V, Unweighted]): G =
    if(e._1 != e._2) {
      val newEdgeLists1 = edgeLists + (e.in -> edgeListFrom(e.in).filterNot { _ ==~ e } )
      val newEdgeLists2 = if(e.isDirected)
          newEdgeLists1
        else
          newEdgeLists1 + (e.out -> edgeListFrom(e.out).filterNot { _ ==~ e })
      newAListGraph(edgeLists)
    } else
      repr
}