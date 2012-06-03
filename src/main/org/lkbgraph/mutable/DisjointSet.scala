package org.lkbgraph.mutable
import scala.annotation.tailrec

/** A class is the implementation of the disjoint set.
 * 
 * @author Łukasz Szpakowski
 */
class DisjointSet[T](val nodeValue: T) extends DisjointSet.OptNode[T]
{ 
  import DisjointSet._
  
  private var mParent: DisjointSet.OptNode[T]= NoNode
    
  private var mRank = 0
          
  @tailrec
  private def find(node: DisjointSet[T]): DisjointSet[T] =
    node.mParent match {
      case NoNode            => node
      case p: DisjointSet[T] => find(p)
    }
    
  /** Returns true if this set contains the set, false otherwise.
   * @param	node		the set.
   * @return			true if this set contains the specified set.
   */
  def contains(node: DisjointSet[T]): Boolean =
    find(this) eq find(node)
  
  /** Combines this set with the specified set.
   * @param node		the set.
   * @return 			this set.
   */
  def += (node: DisjointSet[T]): this.type =  {
    val thisRoot = find(this)
    val nodeRoot = node.find(this)
    if(thisRoot ne nodeRoot) {
      if(thisRoot.mRank < nodeRoot.mRank) {
        thisRoot.mParent = nodeRoot
      } else if(thisRoot.mRank > nodeRoot.mRank) {
        nodeRoot.mParent = thisRoot
      } else {
        nodeRoot.mParent = thisRoot
        thisRoot.mRank += 1
      }
    }
    this
  }
  
  /** Combines this set with the specified sets. */
  def +=(node1: DisjointSet[T], node2: DisjointSet[T], nodes: DisjointSet[T]*): this.type = {
    this += node1 += node2 ++= nodes
  }
  
  /** Combines this set with the specified sets. */
  def ++= (nodes: TraversableOnce[DisjointSet[T]]): this.type = {
    for(node <- nodes) +=(node)
    this
  }
  
  override def equals(that: Any): Boolean =
    that match {
      case ds: DisjointSet[T] => find(this) eq find(ds)
      case _                  => false
    }
  
  override def hashCode: Int =
    find(this).nodeValue.hashCode
  
  override def toString: String =
    "DisjointSet(" + nodeValue + ",...)"
}

/** A singleton for the implementation of the disjoint set.
 * 
 * @author Łukasz Szpakowski
 */
object DisjointSet
{
  trait OptNode[+T]

  case object NoNode extends OptNode[Nothing]
  
  def apply[T](value: T): DisjointSet[T] =    
    new DisjointSet(value)
}