package org.lbkgraph

trait CanFind[-ES, +E, -E1]
{
  def find(es: ES)(e1: E1): Option[E]
}