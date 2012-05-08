package org.lbkgraph

trait Graph[+V, +E <: Product2[V, V]] extends GraphLike[V, E, Graph[V, E]]
{
}