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