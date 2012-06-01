package org.lkbgraph.base.spec
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import org.lkbgraph._
import org.lkbgraph.immutable
import org.lkbgraph._

trait GraphBehaviors[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: base.Graph[XV, XX, XE]] extends GeneratorDrivenPropertyChecks with ShouldMatchers
{
  this: Spec =>

  import spec.GraphParamGen._
  import spec.GraphParamGen.GraphGen._

  def graphFactory: base.GraphFactory[GG]
  
  def graph
  {
    describe("vertices") {
      it("should return all vertices of the graph") {
        forAll(genUnwDiGraphParamsAndExpectData) {
          case (ps, vs, _) => (graphFactory[Char, Unweighted, DiEdge]() ++ ps).vertices.toSet should be === vs
        }
      }
      
      it("should have a size that equals to the number of the vertices") {
        forAll(genUnwDiGraphParamsAndExpectData) {
          case (ps, vs, _) => (graphFactory[Char, Unweighted, DiEdge]() ++ ps).vertices should have size(vs.size)
        }
      }
    }
    
    describe("edges") {
      it("should return all edges of the directed graph") {
        forAll(genUnwDiGraphParamsAndExpectData) { 
          case (ps, _, es) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edges.toSet should be === es
        }
      }
      
      it("should have a size that equals to the number of the edges for directed graph") {
        forAll(genUnwDiGraphParamsAndExpectData) { 
          case (ps, _, es) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edges should have size(es.size)
        }
      }

      it("should return all edges of the undirected graph") {
        forAll(genUnwUndiGraphParamsAndExpectData) { 
          case (ps, _, es) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edges.toSet should be === es
        }
      }
      
      it("should have a size that equals to the number of the edges for undirected graph") {
        forAll(genUnwUndiGraphParamsAndExpectData) { 
          case (ps, _, es) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edges should have size(es.size)
        }
      }
    }
    
    describe("edgesFrom") {
      it("should return the edges from the start vertex for directed graph") {
        forAll(for(t <- genUnwDiGraphParamsAndExpectData; s <- Gen.oneOf(t._2.toSeq)) yield (t, s)) {
          case ((ps, _, es), s) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edgesFrom(s).toSet should be === es.filter { _.in == s }
        }
      }
      
      it("should have a size that equals to the number of the edges from the start vertex for directed graph") {
        forAll(for(t <- genUnwDiGraphParamsAndExpectData; s <- Gen.oneOf(t._2.toSeq)) yield (t, s)) {
          case ((ps, _, es), s) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edgesFrom(s) should have size(es.filter { _.in == s }.size)
        }        
      }
      
      it("should return the edges from the start vertex for undirected graph") {
        forAll(for(t <- genUnwUndiGraphParamsAndExpectData; s <- Gen.oneOf(t._2.toSeq)) yield (t, s)) {
          case ((ps, _, es), s) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edgesFrom(s).toSet should be === es.filter { 
              e => e._1 == s  || e._2 == s 
            }
        }
      }

      it("should have a size that equals to the number of the edges from the start vertex for undirected graph") {
        forAll(for(t <- genUnwUndiGraphParamsAndExpectData; s <- Gen.oneOf(t._2.toSeq)) yield (t, s)) {
          case ((ps, _, es), s) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edgesFrom(s).toSet should have size(es.filter { 
              e => e._1 == s  || e._2 == s 
            }.size)
        }
      }
    }    
  }
}