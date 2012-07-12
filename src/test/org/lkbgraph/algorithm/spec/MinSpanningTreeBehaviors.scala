/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.lkbgraph.algorithm.spec
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.lkbgraph._
import org.lkbgraph.immutable._
import org.lkbgraph.algorithm._
import scala.util.Random
import org.scalacheck.Arbitrary

trait MinSpanningTreeBehaviors[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: base.Graph[XV, XX, XE] with base.GraphLike[XV, XX, XE, _]] extends PropertyChecks with ShouldMatchers
{
  this: Spec =>

  import org.lkbgraph.spec.GraphParamGen._
  import org.lkbgraph.spec.GraphParamGen.GraphGen._
    
  def graphFactory: base.GraphFactory[GG]
  
  def minSpanningTreeStrategy: MinSpanningTreeStrategy
  
  def genMstWUndiEdgesForMst(root: VertexType, vs: Set[VertexType]) = 
    TreeGen.genEdges[Int, Weighted[Int], UndiEdge](Set(root), vs - root) { (v, u, w) =>  v ~ u w w } (Arbitrary { Gen.choose(1, 10) })

  def genGraphWUndiEdgesForMst(vs: Set[VertexType]) = 
    Gen.containerOf[Set, UndiEdge[VertexType, Weighted[Int]]](Gen.pick(2, vs).map2(Gen.choose(30, 40)) { 
      case (Seq(v, u), w) => WUndiEdge[VertexType, Int](v, u, w) 
    }).map { es => es.map { e => (e.toUnweightedEdge, e) }.toMap.values.toSet }
    
  case class MinSpanningTreeParamData[GP, V, E](ps: Set[GP], vs: Set[V], gEs: Set[E], mstEs: Set[E])
  
  def genMinSpanningTreeParamData(root: VertexType, vs: Set[VertexType]) = 
    for {
      mstEs <- genMstWUndiEdgesForMst(root, vs)
      gEs <- genGraphWUndiEdgesForMst(vs).map { es => es.filterNot { e => mstEs.exists { _ ==~ e } } }
    } yield MinSpanningTreeParamData(vs.map(V[VertexType]) ++ gEs ++ mstEs, vs, gEs, mstEs.toSet)
  
  def minSpanningTree
  {
    describe("minSpanningTree") {
      it("should return a tree with one vertex for the empty graph with only one vertex") {
        forAll(Gen.alphaChar) {
          v =>
            val g = (graphFactory[VertexType, Weighted[Int], UndiEdge]() ++ Seq(V(v)))
            val mst = g.minSpanningTree(minSpanningTreeStrategy)
            mst should not be None
            mst.get.vertices.toSet should be === Set(v)
            mst.get.vertices should have size 1
            mst.get.edges.toSet should be === Set()
        }
      }
      
      it("should return a minimum spanning tree for the graph with one connected component") {
        forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq); d <- genMinSpanningTreeParamData(v, vs)) yield d) {
          case MinSpanningTreeParamData(ps, vs, gEs, mstEs) =>
            val g = graphFactory() ++ ps
            //println("mst1")
            val mst = g.minSpanningTree(minSpanningTreeStrategy)
            mst should not be None
            mst.get.vertices.toSet should be === vs
            mst.get.vertices should have  size(vs.size)
            mst.get.edges.toSet should be === mstEs
            mst.get.edges should have size(mstEs.size)
        }
      }
      
      it("should return none for the graph with two connected components") {
        forAll(
          for {
            vs <- genVertices
            v1 <- Gen.oneOf(vs.toSeq)
            v2 <- Gen.oneOf((vs - v1).toSeq)
            n <- Gen.choose(1, vs.size - 1)
            us <- Gen.pick(n, vs.toSeq)
            es1 <- genWUndiEdges(vs -- us)
            es2 <- genWUndiEdges(us.toSet)
          } yield {
            val vs1 = (vs -- us)
            val vs2 = us.toSet
            val ps1 = vs1.map(V[VertexType]).toSet ++ es1
            val ps2 = vs2.map(V[VertexType]).toSet ++ es2
            (GraphParamData(ps1, vs1, es1), GraphParamData(ps2, vs2, es2)) 
          }
        ) {
          case (GraphParamData(ps1, vs1, es1), GraphParamData(ps2, vs2, es2)) =>
            val g = graphFactory() ++ ps1 ++ ps2
            g.minSpanningTree(minSpanningTreeStrategy) should be === None
        }
      }
    }
    
    describe("minSpanningTrees") {
      it("should return a tree with vertices for the empty graph with vertex") {
        forAll(genVertices) {
          vs => 
            val g = graphFactory[VertexType, Weighted[Int], UndiEdge]() ++ vs.map(V[VertexType])
            val msts = g.minSpanningTrees(minSpanningTreeStrategy)
            msts.map { _.vertices.toSet } should be ===(vs.map { v => Set(v) })
            msts.map { _.vertices.size } should be ===(vs.map { v => 1 })
        }
      }
      
      it("should return two minimum spanning trees for the graph with two connected components") {
        forAll {
          for {
            vs <- genVertices
            us <- Gen.someOf(vs)
            v1 <- Gen.oneOf((vs -- us).toSeq)
            v2 <- Gen.oneOf(us.toSeq)
            d1 <- genMinSpanningTreeParamData(v1, vs -- us)
            d2 <- genMinSpanningTreeParamData(v2, us.toSet)
          } yield (d1, d2)
        } {
          case (MinSpanningTreeParamData(ps1, vs1, gEs1, mstEs1), MinSpanningTreeParamData(ps2, vs2, gEs2, mstEs2)) =>
            val g = graphFactory() ++ ps1 ++ ps2
            //println("msts2")
            val msts = g.minSpanningTrees(minSpanningTreeStrategy).toSeq
            msts should have size(2)
            msts(0).vertices.toSet should(be === vs1 or be === vs2)
            msts(0).vertices should(have size(vs1.size) or have size(vs2.size))
            msts(0).edges.toSet should(be === mstEs1 or be === mstEs2)
            msts(0).edges should(have size(mstEs1.size) or have size(mstEs2.size))
            msts(1).vertices.toSet should(be === vs1 or be === vs2)
            msts(1).vertices should(have size(vs1.size) or have size(vs2.size))
            msts(1).edges.toSet should(be === mstEs1 or be === mstEs2)
            msts(1).edges should(have size(mstEs1.size) or have size(mstEs2.size))
            msts(0).vertices should not be msts(1)
        }
      }
    }
  }
}
