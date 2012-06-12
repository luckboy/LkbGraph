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

/** A class is the implementation of the disjoint set.
 * 
 * @author Łukasz Szpakowski
 */
class DisjointSet[T](private val mNode: DisjointSet.Node[T])
{ 
  import DisjointSet._
  
  /** Returns true if this set contains the specified node, false otherwise.
   * @param	node		the node.
   * @return			true if this set contains the specified node.
   */
  def contains(node: Node[T]): Boolean =
    mNode.root eq node.root
  
  /** Adds the specified node to this set.
   * @param node		the set.
   * @return 			this set.
   */
  def += (node: Node[T]): this.type = {
    mNode.union(node)
    this
  }
  
  /** Adds the specified nodes to this set. */
  def +=(node1: Node[T], node2: Node[T], nodes: Node[T]*): this.type = {
    mNode.union(node1).union(node2)
    this ++= nodes
  }
  
  /** Adds the specified nodes to this set. */
  def ++= (nodes: TraversableOnce[Node[T]]): this.type = {
    for(node <- nodes) +=(node)
    this
  }
  
  /** Combines this set with the specified set. 
   * @param set		the set.
   * @return		this set.
   */
  def |= (set: DisjointSet[T]): this.type = {
    this += set.mNode
  }
  
  override def equals(that: Any): Boolean =
    that match {
      case node: Node[T] => mNode.root eq node.root
      case _             => false
    }
  
  override def hashCode: Int =
    mNode.root.hashCode
  
  override def toString: String =
    if(mNode.root eq mNode)
      "DisjointSet(" + mNode.root + ")"
    else
      "DisjointSet(" + mNode.root + ",...)"
}

/** A singleton for the implementation of the disjoint set.
 * 
 * @author Łukasz Szpakowski
 */
object DisjointSet
{
  trait OptNode[+T]

  case object NoNode extends OptNode[Nothing]

  /** A node class for the implementation of the disjoint set.
   * 
   * @author Łukasz Szpakowski
   */
  case class Node[T](val value: T)  extends OptNode[T]
  {
    private var mParent: OptNode[T] = NoNode
    
    private var mRank = 0
          
    private def find(node: Node[T]): Node[T] =
      node.mParent match {
        case NoNode     => node
        case p: Node[T] => 
          val nodeRoot =  find(p)
          node.mParent = nodeRoot
          nodeRoot
      }

    /** The root. */
    def root: Node[T] =
      find(this)
      
    /** Adds the specified node to the set that has this node.
     * @param node		the node.
     * @return			this node.
     */
    def union(node: Node[T]): this.type =  {
      val thisRoot = root
      val nodeRoot = node.root
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
    
    /** Returns a new disjoint set that has this node. */
    def toDisjointSet: DisjointSet[T] =
      new DisjointSet(this)
  }
  
  /** Creates a new disjoint set from nodes.
   * @param node		the first node.
   * @param nodes		the nodes.
   * @return			a new disjoint set.
   */
  def apply[T](node: Node[T], nodes: Node[T]*): DisjointSet[T] =    
    new DisjointSet(node) ++= nodes
}
