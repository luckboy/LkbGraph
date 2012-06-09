package org.lkbgraph.immutable
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.Builder
import org.lkbgraph._
import org.lkbgraph.mutable.TreeBuilder

/** A trait for tree.
 * 
 * @author Łukasz Szpakowski
 */
trait Tree[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends TreeLike[V, X, E, Graph[V, X, E], Tree[V, X, E]] with Graph[V, X, E]
{
  override def newTreeBuilder(root: V): Builder[E[V, X], Tree[V, X, E]] =
    Tree.newTreeBuilder(root)
}

/** A trait for tree.
 * 
 * @author Łukasz Szpakowski
 */
object Tree extends TreeFactory[Tree]
{
  override def treeEmpty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): Tree[V, X, E] =
    new ImplTree(root, Map())
  
  private class ImplTree[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](val root: V, private val mChildEdgeLists: Map[V, List[E[V, X]]]) extends Tree[V, X, E]
  {
    override def vertices: Iterable[V] =
      mChildEdgeLists.keys
    
    override def edges: Iterable[E[V, X]] =
      edgesIterator.toIterable
      
    override def edgesIterator: Iterator[E[V, X]] =
      mChildEdgeLists.toIterator.flatMap { _._2 }

    private def childEdgeListFrom(s: V): List[E[V, X]] =
      mChildEdgeLists.getOrElse(s, Nil)      

    override def childEdgesFrom(s: V): Iterable[E[V, X]] =
      childEdgeListFrom(s)

    private def childEdgeListFromWithEdge(s: V, e: E[V, X]): List[E[V, X]] = {
      val es = childEdgeListFrom(s)
      if(es.exists(e ==~)) es else e :: es
    }      
     
    override def +~^ (e: E[V, X]): Tree[V, X, E] = {
      val e2 = if(!e.isDirected && !containsVertex(e.in)) e.swap else e
      if(containsVertex(e2.out) && !containsVertex(e2.out))
        new ImplTree(root, mChildEdgeLists + (e2.in -> childEdgeListFromWithEdge(e2.in, e2)) + (e2.out -> Nil))
      else
        new ImplTree(root, mChildEdgeLists)
    }
    
    @tailrec
    private def verticesQueue(q: Queue[V], vs: Set[V]): Set[V] =
      q.headOption match {
        case None    => vs
        case Some(v) => verticesQueue(q.tail ++ childrenFrom(v), vs + v)
      }
    
    override def -@^ (s: V): Tree[V, X, E] = {
      val ls = if(containsVertex(s)) mChildEdgeLists -- verticesQueue(Queue(s), Set()) else mChildEdgeLists
      new ImplTree(root, ls + (s -> Nil))
    }
  }
}