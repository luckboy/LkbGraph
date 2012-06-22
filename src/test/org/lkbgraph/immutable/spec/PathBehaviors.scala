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

  trait AddEdgeGens[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]
  {
    def pathPrefix: String
      
    def makeEdge(v: Char, u: Char): E[Char, X]
    
    def genEdges(vs: Set[Char]): Gen[Seq[E[Char, X]]]
    
    def genPathParamData: Gen[PathParamData[Char, E[Char, X]]]

    val genPathParamDataWithoutEdge = for { 
      vs <- genVertices
      us <- Gen.pick(2, vs)
      v <- Gen.oneOf((vs -- us).toSeq)
      es <- genEdges(vs -- us) 
    } yield {
      val Seq(u1, u2) = us
      (PathParamData(es.headOption.map { _._1 }.getOrElse(v), vs -- us, es), makeEdge(u1, u2))
    }
    
    val genPathParamDataWithoutLastEdge = for {
      vs <- genVertices
      u <- Gen.oneOf(vs.toSeq)
      v <- Gen.oneOf((vs - u).toSeq)
      es <- genEdges(vs - u)
      e <- Gen.oneOf(es)
    } yield {
      val e2 = makeEdge(es.lastOption.map { _.out }.getOrElse(v), u)
      (PathParamData(es.headOption.map { _._1 }.getOrElse(v), vs - u, es), e, e2)
    }
  }
  
  implicit object UnwDiAddEdgeGens extends AddEdgeGens[Unweighted, DiEdge]
  {
    override def pathPrefix = "directed" 

    override def makeEdge(v: Char, u: Char) = v -> u
    
    override def genEdges(vs: Set[Char]) = genUnwDiEdges(vs)
    
    override def genPathParamData = genUnwDiPathParamData
  }
  
  implicit object UnwUndiAddEdgeGens extends AddEdgeGens[Unweighted, UndiEdge]
  {
    override def pathPrefix = "undirected" 

    override def makeEdge(v: Char, u: Char) = v ~ u
    
    override def genEdges(vs: Set[Char]) = genUnwUndiEdges(vs)
    
    override def genPathParamData = genUnwUndiPathParamData
  }

  def path
  {
    describe("vertices") {
      it("should return the nodes of the path") {
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
    
    describe("+~/") {
      def edgeAddable[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: AddEdgeGens[X, E]) = {
        import gens._
        
        it("should return a copy of the " + pathPrefix + " path without a new edge that is non-existent") {
          forAll(genPathParamDataWithoutEdge) {
            case (PathParamData(v, vs, es), e) =>
              val p = (pathFactory.newPathBuilder(v) ++= es).result
              val p2 = p +~/ e
              p2.vertices.toSet should be ===(vs)
              p2.vertices should have size(vs.size)
              p2.edges should be ===(es)
          }
        }
        
        it("should return a copy of the " + pathPrefix + " path with a new edge that has the input vertex is last vertex in the path") {
          forAll(genPathParamDataWithoutLastEdge) {
            case (PathParamData(v, vs, es), _, e) => 
              val p = (pathFactory.newPathBuilder(v) ++= es).result
              val p2 = p +~/ e
              p2.vertices.toSet should be ===(vs + e.out)
              p2.vertices should have size(vs.size + 1)
              p2.edges should be ===(es :+ e)
          }
        }

        it("should return a copy of the " + pathPrefix + " path without a new edge that has the input vertex isn't last vertex in the path") {
          forAll(genPathParamDataWithoutLastEdge) {
            case (PathParamData(v, vs, es), e, _) => 
              val p = (pathFactory.newPathBuilder(v) ++= es).result
              val p2 = p +~/ e
              p2.vertices.toSet should be ===(vs)
              p2.vertices should have size(vs.size)
              p2.edges should be ===(es)
          }        	
        }
      }
      
      it should behave like edgeAddable[Unweighted, DiEdge]
      it should behave like edgeAddable[Unweighted, UndiEdge]
      
      it("should return a copy of the directed path with a replaced edge with a new weight") {
        forAll(for(d <- genWDiPathParamData; e <- Gen.oneOf(d.es); dw <- Gen.choose(1, 100)) yield (d, e, dw)) {
          case (PathParamData(v, vs, es), e, dw) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            val e2 = e.in -> e.out w(e.weight ^ dw)
            val p2 = p +~/ e2
            p2.vertices.toSet should be ===(vs)
            p2.vertices should have size(vs.size)
            p2.edges should be ===(es.map { e => if(e ==~ e2) e2 else e})
            p2.edges.toSet should be ===((es.toSet - e) + e2)
        }
      }
      
      it("should return a copy of the undirected path with a replaced edge with a new weight") {
        forAll(for(d <- genWUndiPathParamData; e <- Gen.oneOf(d.es); dw <- Gen.choose(1, 100)) yield (d, e, dw)) {
          case (PathParamData(v, vs, es), e, dw) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            val e2 = e.in ~ e.out w(e.weight ^ dw)
            val p2 = p +~/ e2
            p2.vertices.toSet should be ===(vs)
            p2.vertices should have size(vs.size)
            p2.edges should be ===(es.map { e => if(e ==~ e2) e2 else e})
            p2.edges.toSet should be ===((es.toSet - e) + e2)
        }
      }

      it("should return a copy of the undirected path with a replaced swapped edge with a new weight") {
        forAll(for(d <- genWUndiPathParamData; e <- Gen.oneOf(d.es); dw <- Gen.choose(1, 100)) yield (d, e, dw)) {
          case (PathParamData(v, vs, es), e, dw) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            val e2 = e.out ~ e.in w(e.weight ^ dw)
            val p2 = p +~/ e2
            p2.vertices.toSet should be ===(vs)
            p2.vertices should have size(vs.size)
            p2.edges should be ===(es.map { e => if(e ==~ e2) e2 else e})
            p2.edges.toSet should be ===((es.toSet - e) + e2)
        }        
      }
    }
    
    describe("-@/") {
      it("should return a copy of the path without the unmodified edges and vertices for a non-existent node") {
        forAll(for {
          vs <- genVertices
          v <- Gen.oneOf(vs.toSeq)
          u <- Gen.oneOf((vs - v).toSeq)
          es <- genUnwDiEdges(vs - u)
        } yield (PathParamData(es.headOption.map { _._1 }.getOrElse(v), vs - u, es), u)) {
          case (PathParamData(v, vs, es), u) =>
            val p = (pathFactory.newPathBuilder(v) ++= es).result
            val p2 = p -@/ u
            p2.vertices.toSet should be ===(vs)
            p2.vertices should have size(vs.size)
            p2.edges should be ===(es)
        }
      }
      
      it("should return a copy of the path without the line segment for the edge") {
        forAll(for {
          vs <- genVertices
          (vs1, vs2) <- Gen.someOf(vs.toSeq).map { us => ((vs -- us).toSet,  us.toSet) }
          v1 <- Gen.oneOf(vs1.toSeq)
          v2 <- Gen.oneOf(vs2.toSeq)
          es1 <- genUnwDiEdges(vs1)
          es2 <- genUnwDiEdges(vs2)
        } yield (
            PathParamData(es1.headOption.map { _._1 }.getOrElse(v1), vs1, es1),
            PathParamData(es2.headOption.map { _._1 }.getOrElse(v2), vs2, es2)
            )) {
          case (PathParamData(v1, vs1, es1), PathParamData(v2, vs2, es2)) =>
            val u1 = es1.lastOption.map { _.out }.getOrElse(v1)
            val es =  es1 ++ Seq(u1 -> v2 unw) ++ es2
            val p = (pathFactory.newPathBuilder(v1) ++= es).result
            val p2 = p -@/ v2
            p2.vertices.toSet should be ===(vs1 + v2)
            p2.vertices should have size(vs1.size + 1)
            p2.edges should be ===(es1 :+ (u1 -> v2 unw))
        }
      }
    }
  }
}
