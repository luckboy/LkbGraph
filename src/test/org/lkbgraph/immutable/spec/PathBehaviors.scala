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

trait PathBehaviors[PP[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: Path[XV, XX, XE] with PathLike[XV, XX, XE, _, _, PP[XV, XX, XE]]] extends PropertyChecks with ShouldMatchers
{
  this: Spec =>

  import org.lkbgraph.spec.GraphParamGen._
  import org.lkbgraph.spec.GraphParamGen.PathGen._

  def pathFactory: PathFactory[PP]

  def path
  {
    describe("vertices") {
      it("should return the nodes of the the path") {
        forAll(genUnwDiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.vertices.toSet should be ===(vs)
        }
      }
      
      it("should have the size that equals to the number of the path nodes") {
        forAll(genUnwDiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.vertices should have size(vs.size)
        }        
      }
      
      it("should return the nodes which sorted from the input vertex of the first edge to the output vertex of the last edge") {
        forAll(genUnwDiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.vertices should be ===(Seq(v) ++ es.map { _._2 })
        }        
      }
    }
    
    describe("edges") {
      it("should return the edges in the directed path") {
        forAll(genUnwDiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.edges should be ===(es)
        }
      }

      it("should return the edges in the undirected path") {
        forAll(genUnwUndiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.edges should be ===(es)
        }
      }
    }
    
    describe("childEdgesFrom") {
      it("should return one edge from the first node to the second last node for the directed path") {
        forAll(for(d <- genUnwDiPathParamData; e <- Gen.oneOf(d.es)) yield (d, e)) {
          case (PathParamData(v, vs, es), e) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.childEdgesFrom(e.in).toSet should be === Set(e)
        }
      }

      it("should return the empty sequence for the last node for the directed path") {
        forAll(genUnwDiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.childEdgesFrom(es.lastOption.map { _.out }.getOrElse(v)).toSet should be === Set.empty
        }
      }

      it("should return one edge from the first node to the second last node for the undirected path") {
        forAll(for(d <- genUnwUndiPathParamData; e <- Gen.oneOf(d.es)) yield (d, e)) {
          case (PathParamData(v, vs, es), e) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.childEdgesFrom(e.in).toSet should be === Set(e)
        }
      }

      it("should return the empty sequence for the last node for the undirected path") {
        forAll(genUnwUndiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.childEdgesFrom(es.lastOption.map { _.out }.getOrElse(v)).toSet should be === Set.empty
        }
      }
    }
    
    describe("edgesFrom") {
      it("should return one edge from the first node to the second last node for the directed path") {
        forAll(for(d <- genUnwDiPathParamData; e <- Gen.oneOf(d.es)) yield (d, e)) {
          case (PathParamData(v, vs, es), e) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.edgesFrom(e.in).toSet should be === Set(e)
        }
      }

      it("should return the empty sequence for the last node for the directed path") {
        forAll(genUnwDiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.edgesFrom(es.lastOption.map { _.out }.getOrElse(v)).toSet should be === Set.empty
        }
      }

      it("should return two edges from the second node to the second last node for the undirected path") {
        forAll(for { 
          vs <- genVertices
          v1 <- Gen.oneOf(vs.toSeq)
          v2 <- Gen.oneOf((vs - v1).toSeq)
          v3 <- Gen.oneOf((vs - v1 - v2).toSeq)
          es <- genUnwUndiEdges(vs - v1)
          e <- Gen.oneOf(es)
        } yield {
          val s = es.headOption.map { _.in }.getOrElse(v2)
          (PathParamData(v1, vs, Seq(v1 ~ s) ++ es), e)
        }) {
          case (PathParamData(v, vs, es), e) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.edgesFrom(e.in).toSet should be === (es.find { _.out == e.in }.toSet ++ Set(e))
        }
      }
      
      it("should return one edge for the first node and the undirected path") {
        forAll(genUnwUndiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.edgesFrom(es.headOption.map { _.in }.getOrElse(v)).toSet should be === (es.headOption.toSet)
        }
      }
      
      it("should return one edge for the last node and the undirected path") {
        forAll(genUnwUndiPathParamData) {
          case PathParamData(v, vs, es) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            p.edgesFrom(es.lastOption.map { _.out }.getOrElse(v)).toSet should be === (es.lastOption.toSet)
        }
      }
    }
  }
}
