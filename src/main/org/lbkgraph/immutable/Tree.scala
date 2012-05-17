package org.lbkgraph.immutable
import org.lbkgraph._

/** A trait for the immutable tree.
 * 
 * @author Łukasz Szpakowski
 */
trait Tree[V, E <: EdgeLike[V, E]] extends TreeLike[V, E, Graph[V, E], Tree[V, E]] with Graph[V, E]
