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
  def pathEmpty[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](root: V): Path[V, X, E] =
    new ImplPath(root, Seq())
  
  private class ImplPath[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](val root: V, val edgeSeq: Seq[E[V, X]]) extends Path[V, X, E]
  {
    override def +~/ (e: E[V, X]): Path[V, X, E] = {
      val v = edgeSeq.lastOption.map { le => le.out }.getOrElse(root)
      val e2 = if(!e.isDirected && e.in != v) e.swap else e
      new ImplPath(root, edgeSeq :+ e)
    }
      
    override def -@/ (s: V): Path[V, X, E] =
      new ImplPath(root, edgeSeq.takeWhile { e => e.in != s })
  }
}