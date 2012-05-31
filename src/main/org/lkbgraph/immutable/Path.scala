package org.lkbgraph.immutable
import org.lkbgraph._

/** A trait for path.
 * 
 * @author Łukasz Szpakowski
 */
trait Path[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends PathLike[V, X, E, Graph[V, X, E], Path[V, X, E]] with Tree[V, X, E]
