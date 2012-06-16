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

package org.lkbgraph.immutable.spec
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.lkbgraph._
import org.lkbgraph.immutable._

trait TreeBehaviors[TT[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: Tree[XV, XX, XE] with TreeLike[XV, XX, XE, _, TT[XV, XX, XE]]] extends PropertyChecks with ShouldMatchers
{
  this: Spec =>

  import org.lkbgraph.spec.GraphParamGen._
  import org.lkbgraph.spec.GraphParamGen.TreeGen._

  def treeFactory: TreeFactory[TT]

  def tree
  {
    describe("vertices") {
      it("should return all nodes of the tree") {
        forAll(genUnwDiTreeParamData) {
          case TreeParamData(v, _, es) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.vertices.toSet should be === (es.flatMap { e => Set(e.in, e.out) } + v)
        }
      }
      
      it("should have the size that equals to the number of the nodes") {
        forAll(genUnwDiTreeParamData) {
          case TreeParamData(v, _, es) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.vertices should have size(es.size + 1)
        }
      }
    }
    
    describe("edges") {
      it("should return all edges of the directed tree") {
        forAll(genUnwDiTreeParamData) {
          case TreeParamData(v, _, es) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edges.toSet should be === es
        }
      }
      
      it("should have the size that equals to the number of the edges for the directed tree") {
        forAll(genUnwDiTreeParamData) {
          case TreeParamData(v, _, es) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edges should have size(es.size)
        }
      }

      it("should return all edges of the undirected tree") {
        forAll(genUnwUndiTreeParamData) {
          case TreeParamData(v, _, es) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edges.toSet should be === es
        }
      }
      
      it("should have the size that equals to the number of the edges for the undirected tree") {
        forAll(genUnwUndiTreeParamData) {
          case TreeParamData(v, _, es) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edges should have size(es.size)
        }
      }
    }
    
    describe("childEdgesFrom") {
      it("should return the edges from the start vertex for the directed tree") {
        forAll(for(d <- genUnwDiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.childEdgesFrom(u).toSet should be ===(es.filter { _.in == u })
        }
      }

      it("should have the size that equals to the number of the edges from the start vertex for the directed tree") {
        forAll(for(d <- genUnwDiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.childEdgesFrom(u) should have size(es.filter { _.in == u }.size)
        }
      }

      it("should return the child edges from the start vertex for the undirected tree") {
        forAll(for(d <- genUnwUndiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.childEdgesFrom(u).toSet should be ===(es.filter { _.in == u })
        }
      }

      it("should have the size that equals to the number of the edges from the start vertex for the undirected tree") {
        forAll(for(d <- genUnwDiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.childEdgesFrom(u) should have size(es.filter { _.in == u }.size)
        }
      }
    }
    
    describe("edgesFrom") {
      it("should return the edges from the start vertex for the directed tree") {
        forAll(for(d <- genUnwDiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edgesFrom(u).toSet should be ===(es.filter { _.in == u })
        }
      }

      it("should have the size that equals to the number of the edges from the start vertex for the directed tree") {
        forAll(for(d <- genUnwDiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edgesFrom(u) should have size(es.filter { _.in == u }.size)
        }
      }

      it("should return the edges from the start vertex for the undirected tree") {
        forAll(for(d <- genUnwUndiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edgesFrom(u).toSet should be ===(es.filter { e => e.in == u || e.out == u })
        }
      }

      it("should have the size that equals to the number of the edges from the start vertex for the undirected tree") {
        forAll(for(d <- genUnwUndiTreeParamData; u <- Gen.oneOf(d.vs.toSeq)) yield (d, u)) {
          case (TreeParamData(v, _, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            t.edgesFrom(u) should have.size(es.filter { e => e.in == u || e.out == u }.size)
        }
      }
    }
    
    describe("+~^") {
      
    }
    
    describe("-@^") {
      
    }
  }
}
