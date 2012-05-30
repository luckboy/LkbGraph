package org.lkbgraph.immutable
import org.lkbgraph._

/** A trait for the immutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait Tree[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends TreeLike[V, X, E, Graph[V, X, E], Tree[V, X, E]] with Graph[V, X, E]
