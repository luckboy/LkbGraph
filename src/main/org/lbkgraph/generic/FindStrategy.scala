package org.lbkgraph.generic

trait FindStrategy[-ES, +E, -E1]
{
  def find(es: ES)(e1: E1): Option[E]
}