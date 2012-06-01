package org.lkbgraph.immutable.spec
import org.scalatest.Spec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.lkbgraph._
import org.lkbgraph.immutable._

@RunWith(classOf[JUnitRunner])
class AdjListGraphSpec extends Spec with base.spec.GraphBehaviors[AdjListGraph]
{
  override def graphFactory: base.GraphFactory[AdjListGraph] = AdjListGraph
  
  describe("An AdjListGraph") {
    it should behave like graph
  }
}