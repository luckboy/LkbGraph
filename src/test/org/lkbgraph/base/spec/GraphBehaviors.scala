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

package org.lkbgraph.base.spec
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.lkbgraph._
import org.lkbgraph.immutable._

trait GraphBehaviors[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: base.Graph[XV, XX, XE]] extends PropertyChecks with ShouldMatchers
{
  this: Spec =>

  import org.lkbgraph.spec.GraphParamGen._
  import org.lkbgraph.spec.GraphParamGen.GraphGen._

  def graphFactory: base.GraphFactory[GG]
  
  trait AddSubGens[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]
  {
    def graphPrefix: String
      
    def makeEdge(v: Char, u: Char): E[Char, X]
      
    def genGraphParamDataWithoutEdgeAndVertices: Gen[(GraphParamData[GraphParam[Char, X, E], Char, E[Char, X]], E[Char, X])] =
      for {
        vs <- genVertices; us <- Gen.pick(2, vs); es <- genEdges(vs -- us)
      } yield {
        val Seq(v, u) = us
        val vs2 = vs -- us
        (GraphParamData(vs2.map(V[Char]) ++ es, vs2, es), makeEdge(v, u)) 
      }
        
    def genGraphParamData: Gen[GraphParamData[GraphParam[Char, X, E], Char, E[Char, X]]]
      
    def genEdges(vs: Set[Char]): Gen[Set[E[Char, X]]]
  }
    
  implicit object UnwDiAddSubGens extends AddSubGens[Unweighted, DiEdge]
  {
    override def graphPrefix: String = "directed"

    override def makeEdge(v: Char, u: Char) = v -> u unw
      
    override def genGraphParamData = genUnwDiGraphParamData
      
    override def genEdges(vs: Set[Char]) = genUnwDiEdges(vs)
  }

  implicit object UnwUndiAddSubGens extends AddSubGens[Unweighted, UndiEdge]
  {
    override def graphPrefix: String = "undirected"

    override def makeEdge(v: Char, u: Char) = v ~ u
      
    override def genGraphParamData = genUnwUndiGraphParamData
      
    override def genEdges(vs: Set[Char]) = genUnwUndiEdges(vs)
  }
  
  val genUnwDiGraphParamDataWithoutConnectedVertex = for { 
    vs <- genVertices
    v <- Gen.oneOf(vs.toSeq)
    es <- genUnwDiEdges(vs - v)
    us <- Gen.someOf(vs - v) 
  } yield { 
    val vs2 = vs - v
    (GraphParamData(vs2.map(V[Char]) ++ es, vs2, es), (v, us.map { UnwDiEdge[Char](v, _) }.toSet))
  }
  
  def graph
  {
    describe("vertices") {
      it("should return all vertices of the graph") {
        forAll(genUnwDiGraphParamData) {
          case GraphParamData(ps, vs, _) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).vertices.toSet should be === vs
        }
      }
      
      it("should have a size that equals to the number of the vertices") {
        forAll(genUnwDiGraphParamData) {
          case GraphParamData(ps, vs, _) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).vertices should have size(vs.size)
        }
      }
    }
    
    describe("edges") {
      it("should return all edges of the directed graph") {
        forAll(genUnwDiGraphParamData) { 
          case GraphParamData(ps, _, es) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edges.toSet should be === es
        }
      }
      
      it("should have a size that equals to the number of the edges for directed graph") {
        forAll(genUnwDiGraphParamData) { 
          case GraphParamData(ps, _, es) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edges should have size(es.size)
        }
      }

      it("should return all edges of the undirected graph") {
        forAll(genUnwUndiGraphParamData) { 
          case GraphParamData(ps, _, es) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edges.toSet should be === es
        }
      }
      
      it("should have a size that equals to the number of the edges for undirected graph") {
        forAll(genUnwUndiGraphParamData) { 
          case GraphParamData(ps, _, es) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edges should have size(es.size)
        }
      }
    }
    
    describe("edgesFrom") {
      it("should return the edges from the start vertex for directed graph") {
        forAll(for(t <- genUnwDiGraphParamData; s <- Gen.oneOf(t.vs.toSeq)) yield (t, s)) {
          case (GraphParamData(ps, _, es), s) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edgesFrom(s).toSet should be === es.filter { _.in == s }
        }
      }
      
      it("should have a size that equals to the number of the edges from the start vertex for directed graph") {
        forAll(for(t <- genUnwDiGraphParamData; s <- Gen.oneOf(t.vs.toSeq)) yield (t, s)) {
          case (GraphParamData(ps, _, es), s) => 
            (graphFactory[Char, Unweighted, DiEdge]() ++ ps).edgesFrom(s) should have size(es.filter { _.in == s }.size)
        }        
      }
      
      it("should return the edges from the start vertex for undirected graph") {
        forAll(for(t <- genUnwUndiGraphParamData; s <- Gen.oneOf(t.vs.toSeq)) yield (t, s)) {
          case (GraphParamData(ps, _, es), s) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edgesFrom(s).toSet should be === es.filter { 
              e => e._1 == s || e._2 == s 
            }
        }
      }

      it("should have a size that equals to the number of the edges from the start vertex for undirected graph") {
        forAll(for(t <- genUnwUndiGraphParamData; s <- Gen.oneOf(t.vs.toSeq)) yield (t, s)) {
          case (GraphParamData(ps, _, es), s) => 
            (graphFactory[Char, Unweighted, UndiEdge]() ++ ps).edgesFrom(s).toSet should have size(es.filter { 
              e => e._1 == s || e._2 == s 
            }.size)
        }
      }
    }    
    
    describe("+@") {
      it("should return a copy of the graph with a new vertex for a non-existent vertex") {
    	forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
    	  case (vs, v) => 
    	    val ps = (vs - v).map(V[Char])
    	    val g = graphFactory[Char, Unweighted, DiEdge]() ++ ps
    	    val g2 = g +@ v
    	    g2.vertices.toSet should be === vs
    	    g2.vertices should have size(vs.size)
    	}
      }
      
      it("should return a graph with the unmodified vertex set for the vertex at the graph") {
    	forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
    	  case (vs, v) => 
    	    val ps = vs.map(V[Char])
            val g = graphFactory[Char, Unweighted, DiEdge]() ++ ps
    	    val g2 = g +@ v
    	    g2.vertices.toSet should be === vs
    	    g2.vertices should have size(vs.size)
    	}
      }
    }
    
    describe("+~") {
      def addable[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: AddSubGens[X, E]) = {
        import gens._
        
        it("should return a " + graphPrefix + " graph with a modified vertice set and a modified edge set for a new edge that vertices isn't exists") {
          forAll(genGraphParamDataWithoutEdgeAndVertices) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              val g2 = g +~ e
              g2.vertices.toSet should be === ((vs + e._1) + e._2)
              g2.vertices should have size(vs.size + 2)
              g2.edges.toSet should be === (es + e)
              g2.edges should have size(es.size + 1)
          }
        }

        it("should return a " +graphPrefix + " graph with a modified edge set for a new edge that vertices is exists") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ (ps - e)
              val g2 = g +~ e
              g2.vertices.toSet should be === vs
              g2.vertices should have size(vs.size)
              g2.edges.toSet should be === es
              g2.edges should have size(es.size)
          }
        }

        it("should return a " +graphPrefix + "graph with the unmodified edge set for the edge in the graph") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              val g2 = g +~ e
              g2.vertices.toSet should be === vs
              g2.vertices should have size(vs.size)
              g2.edges.toSet should be === es
              g2.edges should have size(es.size)
          }
        }         
      }
      
      it should behave like addable[Unweighted, DiEdge]
      it should behave like addable[Unweighted, UndiEdge]
    }

    describe("-@") {
      it("should return a copy of the graph with unmodified vertices for a non-existent vertex") {
    	forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
    	  case (vs, v) => 
    	    val ps = (vs - v).map(V[Char])
    	    val g = graphFactory[Char, Unweighted, DiEdge]() ++ ps
    	    val g2 = g -@ v
    	    g2.vertices.toSet should be === (vs - v)
    	    g2.vertices should have size(vs.size - 1)
    	}
      }
      
      it("should return a copy of the graph wihout the vertex that been in the graph") {
        forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
          case (vs, v) =>
            val ps = vs.map(V[Char])
            val g = graphFactory[Char, Unweighted, DiEdge]() ++ ps
            val g2 = g -@ v
            g2.vertices.toSet should be === (vs - v)
            g2.vertices should have size(vs.size - 1)
        }
      }
      
      it("should return a copy of the graph without the edges which are connected to the removed vertex") {
        forAll(genUnwDiGraphParamDataWithoutConnectedVertex) {
          case (GraphParamData(ps, vs, es), (v, es2)) =>
            val ps2 = ps + V(v) ++ es2
            val g = graphFactory[Char, Unweighted, DiEdge]() ++ ps2
            val g2 = g -@ v
            //println(ps, vs, es, v, es2)
            //println(g)
            //println(g2)
            g2.vertices.toSet should be === (vs - v)
            g2.vertices should have size(vs.size)
            g2.edges.toSet should be === es 
            g2.edges should have size(es.size)
        }
      }
    }

    describe("-~!") {
      def substrctable[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: AddSubGens[X, E]) = {
        import gens._
        
        it("should return a copy of the " + graphPrefix + " graph with the unmodified edge set and the unmodified vertex set for a non-existent edge") {
          forAll(genGraphParamDataWithoutEdgeAndVertices) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              val g2 = g -~! e.toUnweightedEdge
              g2.vertices.toSet should be === vs
              g2.vertices should have size(vs.size)
              g2.edges.toSet should be === (es)
              g2.edges should have size(es.size)
          }
        }
      
        it("should return a copy of the " + graphPrefix + " graph with unmodified edge set and the umodified vertex set for a non-existent edge that has vertices in the graph") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ (ps - e)
              val g2 = g -~! e.toUnweightedEdge
              g2.vertices.toSet should be === vs
              g2.vertices should have size(vs.size)
              g2.edges.toSet should be === (es - e)
              g2.edges should have size(es.size - 1)
          }
        }

        it("should return a copy of the " + graphPrefix +" graph without the edge for the edge is exists") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              val g2 = g -~! e.toUnweightedEdge
              g2.vertices.toSet should be === vs
              g2.vertices should have size(vs.size)
              g2.edges.toSet should be === (es - e)
              g2.edges should have size(es.size - 1)
          }
        }
      }

      it should behave like substrctable[Unweighted, DiEdge]

      it should behave like substrctable[Unweighted, UndiEdge]
    }
  }
}
