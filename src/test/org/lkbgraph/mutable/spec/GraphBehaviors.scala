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

package org.lkbgraph.mutable.spec
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.lkbgraph._
import org.lkbgraph.mutable._

trait GraphBehaviors[GG[XV, XX, XE[+XY, +XZ] <: EdgeLike[XY, XZ, XE]] <: Graph[XV, XX, XE]] extends base.spec.GraphBehaviors[GG]
{
  this: Spec =>

  import org.lkbgraph.spec.GraphParamGen._
  import org.lkbgraph.spec.GraphParamGen.GraphGen._

  def mutableGraph
  {
    describe("+@=") {
      it("should add a new vertex onto the graph") {
        forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
          case (vs, v) =>
            val g = graphFactory[Char, Unweighted, DiEdge]() ++ (vs - v).map(V[Char])
            g +@= v
            g.vertices.toSet should be === vs
            g.vertices should have size(vs.size)
        }
      }
      
      it("shouldn't add the exitent vertex in the graph onto the graph") {
        forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
          case (vs, v) =>
            val g = graphFactory[Char, Unweighted, DiEdge]() ++ vs.map(V[Char])
            g +@= v
            g.vertices.toSet should be === vs
            g.vertices should have size(vs.size)
        }        
      }
    }
    
    describe("+~=") {
      def growable[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: AddSubGens[X, E]) = {
        import gens._
        
        it("should add a new edge that vertices is a non-existent onto the " + gens.graphPrefix + " graph") {
          forAll(genGraphParamDataWithoutEdgeAndVertices) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              g +~= e
              g.vertices.toSet should be === ((vs + e._1) + e._2)
              g.vertices should have size(vs.size + 2)
              g.edges.toSet should be === (es + e)
              g.edges should have size(es.size + 1)

          }
        }
        
        it("should add a new edge that vertices is the existent onto the " + gens.graphPrefix + " graph") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ (ps - e)
              g +~= e
              g.vertices.toSet should be === (vs)
              g.vertices should have size(vs.size)
              g.edges.toSet should be === (es)
              g.edges should have size(es.size)
          }
        }
        
        it("shouldn't add the existent edge onto the " + gens.graphPrefix + " graph") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              g +~= e
              g.vertices.toSet should be === (vs)
              g.vertices should have size(vs.size)
              g.edges.toSet should be === (es)
              g.edges should have size(es.size)
          }
        }
      }

      it should behave like growable[Unweighted, DiEdge]
      it should behave like growable[Unweighted, UndiEdge]
    }
    
    describe("-@=") {
      it("shouldn't remove the a non-existent vertex from the graph ") {
    	forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
    	  case (vs, v) => 
    	    val g = graphFactory[Char, Unweighted, DiEdge]() ++ (vs - v).map(V[Char])
    	    g -@= v
    	    g.vertices.toSet should be === (vs - v)
    	    g.vertices should have size(vs.size - 1)
    	}        
      }

      it("should remove the existent vertex that from the graph") {
    	forAll(for(vs <- genVertices; v <- Gen.oneOf(vs.toSeq)) yield (vs, v)) {
    	  case (vs, v) => 
    	    val g = graphFactory[Char, Unweighted, DiEdge]() ++ (vs - v).map(V[Char])
    	    g -@= v
    	    g.vertices.toSet should be === (vs - v)
    	    g.vertices should have size(vs.size - 1)
    	}
      }
      
      it("should remove the specified vertex and the connected edges with the specified vertex from the graph") {
        forAll(genUnwDiGraphParamDataWithoutConnectedVertex) {
          case (GraphParamData(ps, vs, es), (v, es2)) =>
            val ps2 = ps + V(v) ++ es2
            val g = graphFactory[Char, Unweighted, DiEdge]() ++ ps2
            g -@= v
            g.vertices.toSet should be === (vs - v)
            g.vertices should have size(vs.size)
            g.edges.toSet should be === es 
            g.edges should have size(es.size)
        }        
      }
    }
    
    describe("-~=!") {
      def shrinkable[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: AddSubGens[X, E]) = {
        import gens._

        it("shouldn't remove a non-existent edge from the " + graphPrefix + " graph") {
          forAll(genGraphParamDataWithoutEdgeAndVertices) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              g -~!= e.toUnweightedEdge
              g.vertices.toSet should be === vs
              g.vertices should have size(vs.size)
              g.edges.toSet should be === (es)
              g.edges should have size(es.size)
          }
        }
        
        it("shouldn't remove a non-existent edge that has vertices in the " + graphPrefix + " graph from the " + graphPrefix + " graph") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ (ps - e)
              g -~!= e.toUnweightedEdge
              g.vertices.toSet should be === vs
              g.vertices should have size(vs.size)
              g.edges.toSet should be === (es - e)
              g.edges should have size(es.size - 1)
          }
        }

        it("should remove th existent edge from the " + graphPrefix + " graph") {
          forAll(for(t <- genGraphParamData; e <- Gen.oneOf(t.es.toSeq)) yield (t, e)) {
            case (GraphParamData(ps, vs, es), e) =>
              val g = graphFactory[Char, X, E]() ++ ps
              g -~!= e.toUnweightedEdge
              g.vertices.toSet should be === vs
              g.vertices should have size(vs.size)
              g.edges.toSet should be === (es - e)
              g.edges should have size(es.size - 1)
          }
        }  
      }
      
      it should behave like shrinkable[Unweighted, DiEdge]
      it should behave like shrinkable[Unweighted, UndiEdge]
    }
  }
}
