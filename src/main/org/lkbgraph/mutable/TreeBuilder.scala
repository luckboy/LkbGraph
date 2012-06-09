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
    if(!elem.isDirected) (elem.out)
    this
  }
	
  override def clear(): Unit =
    mEdges.clear()  
    
  override def result(): T =
    (1 to mEdges.size).reverse.foldLeft(empty, mEdges.clone()) {
      case ((t, es), _) => 
        t.vertices.find { v => es.contains(v) }.flatMap { v => es.get(v).map { e => (t +~^ e, es -= v) } }.getOrElse(t, es)
    }._1
}