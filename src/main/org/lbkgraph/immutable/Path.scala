package org.lbkgraph.immutable
import org.lbkgraph._

/** A trait for the path.
 * 
 * @author Łukasz Szpakowski
 */
trait Path[V, E <: EdgeLike[V, E]] extends PathLike[V, E, Graph[V, E], Path[V, E]] with Tree[V, E]
