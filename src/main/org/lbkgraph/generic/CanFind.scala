package org.lbkgraph.generic

trait CanFind[-ES, +E, -E1]
{
  def apply(): FindStrategy[ES, E, E1]
}