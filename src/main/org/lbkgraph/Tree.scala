package org.lbkgraph

trait Tree[+V, +E <: Product2[V, V]] extends Graph[V, E] with TreeLike[V, E, Tree[V, E]]
{
}