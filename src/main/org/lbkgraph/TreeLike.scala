package org.lbkgraph

trait TreeLike[+V, +E <: Product2[V, V], +T] extends GraphLike[V, E, T] 
{
  /** The nodes is the synonym of vertices. */
  def nodes: Set[V]
  
  /** The root. */
  def root: V
  
  def preOrder: Seq[V] =
    preOrderFrom(root)
    
  def postOrder: Seq[V] =
    postOrderFrom(root)

  def levelOrder: Seq[V] =
    preOrderFrom(root)

  def preOrderFrom(s: V): Seq[V] = throw new Exception
  
  def postOrderFrom(s: V): Seq[V] = throw new Exception
}