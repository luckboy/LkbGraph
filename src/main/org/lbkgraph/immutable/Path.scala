package org.lbkgraph.immutable
import org.lbkgraph._

/** A trait for the path.
 * 
 * @author Łukasz Szpakowski
 */
trait Path[V, X, E[+Y, +Z] <: EdgeLike[Y, Z, E]] extends PathLike[V, X, E, Graph[V, X, E], Path[V, X, E]] with Tree[V, X, E]
