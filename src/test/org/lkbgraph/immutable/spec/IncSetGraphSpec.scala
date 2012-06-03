package org.lkbgraph.immutable.spec
import org.scalatest.Spec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.lkbgraph._
import org.lkbgraph.immutable._

@RunWith(classOf[JUnitRunner])
class IncSetGraphSpec extends Spec with base.spec.GraphBehaviors[IncSetGraph]
{
  override def graphFactory: base.GraphFactory[IncSetGraph] = IncSetGraph
  
  describe("An IncSetGraph") {
    it should behave like graph
  }
}