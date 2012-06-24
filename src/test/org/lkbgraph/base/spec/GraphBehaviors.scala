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
    us2 <- Gen.someOf((vs -- us) - v)
  } yield { 
    val vs2 = vs - v
    (GraphParamData(vs2.map(V[Char]) ++ es, vs2, es), (v, us.map { UnwDiEdge[Char](v, _) }.toSet ++ us2.map { UnwDiEdge[Char](_, v) }.toSet))
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
      
      it("should return a directed graph with the replaced edge with weight for a new weight") {
        forAll(for(t <- genWDiGraphParamData; e <- Gen.oneOf(t.es.toSeq); dw <- Gen.choose(1, 1000)) yield (t, e, dw)) {
          case (GraphParamData(ps, vs, es), e, dw) => 
            val g = graphFactory[Char, Weighted[Int], DiEdge]() ++ ps
            val e2 = e.in -> e.out w (e.weight ^ dw)
            val g2 = g +~ e2
            g2.vertices.toSet should be === vs
            g2.vertices should have size(vs.size)
            g2.edges.toSet should be === ((es - e) + e2)
            g2.edges should have size(es.size)
        }
      }

      it("should return a undirected graph with the replaced edge with weight for a new weight") {
        forAll(for(t <- genWUndiGraphParamData; e <- Gen.oneOf(t.es.toSeq); dw <- Gen.choose(1, 1000)) yield (t, e, dw)) {
          case (GraphParamData(ps, vs, es), e, dw) => 
            val g = graphFactory[Char, Weighted[Int], UndiEdge]() ++ ps
            val e2 = e.in ~ e.out w (e.weight ^ dw)
            val g2 = g +~ e2
            g2.vertices.toSet should be === vs
            g2.vertices should have size(vs.size)
            g2.edges.toSet should be === ((es - e) + e2)
            g2.edges should have size(es.size)
        }
      }

      it("should return a undirected graph with the replaced swaped edge with weight for a new weight") {
        forAll(for(t <- genWUndiGraphParamData; e <- Gen.oneOf(t.es.toSeq); dw <- Gen.choose(1, 1000)) yield (t, e, dw)) {
          case (GraphParamData(ps, vs, es), e, dw) => 
            val g = graphFactory[Char, Weighted[Int], UndiEdge]() ++ ps
            val e2 = e.out ~ e.in w (e.weight ^ dw)
            val g2 = g +~ e2
            g2.vertices.toSet should be === vs
            g2.vertices should have size(vs.size)
            g2.edges.toSet should be === ((es - e) + e2)
            g2.edges should have size(es.size)
        }
      }
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
    
    it should behave like dfs
    
    it should behave like bfs
  }
  
  case class Edges123[V, E](v: V, es1: Seq[E], es12: Seq[E], es10: Seq[E], es11: Set[E], es20: Seq[E], es21: Seq[E], es22: Set[E])
    
  val genUnwDiEdges123 = for {
	vs <- genVertices
	u0 <- Gen.oneOf(vs.toSeq)
	n1 <- Gen.choose(1, vs.size)
	us1 <- Gen.pick(n1, (vs - u0).toSeq)
      
	n12 <- Gen.choose(1, (vs - u0 -- us1).size)
	vs12 <- Gen.listOfN(n12, Gen.oneOf(us1))
	us12 <- Gen.pick(n12, (vs - u0 -- us1).toSeq)
      
	n10 <- Gen.choose(1, us1.size)
	us10 <- Gen.pick(n10, us1)
	
	es111 <- Gen.containerOf[Set, DiEdge[VertexType, Unweighted]](Gen.pick(2, us1).map { case Seq(v, u) => v -> u unw })
	es112 <- Gen.containerOf[Set, DiEdge[VertexType, Unweighted]](Gen.pick(2, us1).map { case Seq(v, u) => u -> v unw })
	
	n20 <- Gen.choose(1, us12.size)
	us20 <- Gen.pick(n20, us12)
	
	n21 <- Gen.choose(1, us12.size)
	vs21 <- Gen.listOfN(n21, Gen.oneOf(us1))
	us21 <- Gen.pick(n12, us12.toSeq)
    	
	es221 <- Gen.containerOf[Set, DiEdge[VertexType, Unweighted]](Gen.pick(2, us12).map { case Seq(v, u) => v -> u unw })
	es222 <- Gen.containerOf[Set, DiEdge[VertexType, Unweighted]](Gen.pick(2, us12).map { case Seq(v, u) => u -> v unw })
  } yield {
	val es1 = us1.map { u0 -> _ unw }
	val es12 = vs12.zip(us12).map { case (v, u) => v -> u unw }
	val es10 = us10.map { _ -> u0 unw }
	val es20 = us20.map { _ -> u0 unw }
	val es21 = vs21.zip(us21).map { case (v, u) => u -> v unw }
	Edges123(u0, es1, es12, es10, es111 ++ es112, es20, es21, es221 ++ es222)
  }

  case class TriangleEdges[V, E](v: V, es12: Seq[E], es13: Seq[E], es23: Seq[E], es32: Seq[E])
  
  val genUnwDiTriangleEdges = for {
    vs <- genVertices
    v1 <- Gen.oneOf(vs.toSeq)
    n1 <- Gen.choose(1, vs.size - 3)
    us1 <- Gen.pick(n1, vs - v1)
    n2 <- Gen.choose(1, (vs -- us1).size - 2)
    us2 <- Gen.pick(n2, vs -- us1 - v1)
    n3 <- Gen.choose(1, (vs -- us1 -- us2).size - 1)
    us3 <- Gen.pick(n2, vs -- us1 -- us2 - v1)    
  } yield {
    //    1
    //   / \
	//  V   V
    //  2 ~ 3
    val es12 = (Seq(v1) ++ us1.init).zip(us1).map { case (v, u) => v -> u unw }
    val es13 = (Seq(v1) ++ us3.init).zip(us3).map { case (v, u) => v -> u unw }
    val es23 = (Seq(us1.last) ++ us2).zip(us2 ++ Seq(us3.last)).map { case (v, u) => v -> u unw }
    val es32 = (Seq(us3.last) ++ us2.reverse).zip(us2.reverse ++ Seq(us1.last)).map { case (v, u) => v -> u unw }
    TriangleEdges(v1, es12, es13, es23, es32)
  }
  
  val genUnwDiGraphParamData3 = for {
    vs <- genVertices

    vs1 <- Gen.someOf(vs.toSeq)
    v1 <- Gen.oneOf(vs1.toSeq)
    es1 <- genUnwDiEdges(vs1.toSet)
    fs1 <- TreeGen.genUnwDiEdges(v1, vs1.toSet)
    
    vs2 <- Gen.someOf((vs -- vs1).toSeq)
    v2 <- Gen.oneOf(vs2.toSeq)
    es2 <- genUnwDiEdges(vs2.toSet)
    fs2 <- TreeGen.genUnwDiEdges(v2, vs2.toSet)

    vs3 <- Gen.someOf((vs -- vs1 -- vs2).toSeq)
    v3 <- Gen.oneOf(vs3.toSeq)
    es3 <- genUnwDiEdges(vs3.toSet)
    fs3 <- TreeGen.genUnwDiEdges(v3, vs3.toSet)
  } yield (
      (GraphParamData(vs1.map(V[VertexType]).toSet ++ es1 ++ fs1, vs1.toSet, es1 ++ fs1), v1), 
      (GraphParamData(vs2.map(V[VertexType]).toSet ++ es2 ++ fs2, vs2.toSet, es2 ++ fs2), v2), 
      (GraphParamData(vs3.map(V[VertexType]).toSet ++ es3 ++ fs3, vs3.toSet, es3 ++ fs3), v3)
      )
      
  case class DiamondEdges[V, E](v1: V, es12: Seq[E], es23: Seq[E], es43: Seq[E], es54: Seq[E], v5: V)
  
  val genUnwDiDiamondEdges = for {
    vs <- genVertices
    v1 <- Gen.oneOf(vs.toSeq)
    v5 <- Gen.oneOf((vs - v1).toSeq)

    n2 <- Gen.choose(1, ((vs - v1) - v5).size)
    us2 <- Gen.pick(n2, ((vs - v1) - v5).toSeq)
    us3 <- Gen.pick(n2, (((vs -- us2) - v1) - v5).toSeq)
    us4 <- Gen.pick(n2, ((((vs -- us3) -- us2) - v1) - v5).toSeq)
  } yield {
    val es12 = us2.map { v1 -> _ unw }
    val es54 = us4.map { v5 -> _ unw }
    val es23 = us2.zip(us3).map { case (v, u) => v -> u unw }
    val es43 = us4.zip(us3).map { case (v, u) => v -> u unw }
    DiamondEdges(v1, es12, es23, es43, es54, v5)
  }
  
  def dfs
  {
    describe("dfsFrom") {
      it("should return a tree with the first vertices from the start vertex and the second vertices from the first vertices") {
        forAll(genUnwDiEdges123) {
          case Edges123(v, es1, es12, _, _, _, _, _) =>
            val g = (graphFactory() ++ es1 ++ es12)
            val t = g.dfsFrom(v)
            t.childEdgesFrom(v).toSet should be ===(es1.toSet)
            t.childEdgesFrom(v) should have size(es1.size)
            es1.flatMap { e => t.childEdgesFrom(e.out) }.toSet should be ===(es12.toSet)
            es1.flatMap { e => t.childEdgesFrom(e.out) }.toSet should have size(es12.size)
        }
      }
      
      it("should return a tree with two branches for the triangle graph") {
        forAll(genUnwDiTriangleEdges) {
          case TriangleEdges(v, es12, es13, es23, es32) =>
            val g = graphFactory() ++ es12 ++ es13 ++ es23 ++ es32
            val t = g.dfsFrom(v)
            t.edges.toSet should(be ===((es12 ++ es13.init ++ es23).toSet) or be ===((es12.init ++ es13 ++ es32).toSet))
            t.edges should(have size((es12 ++ es13.init ++ es23).size) or have size((es12.init ++ es13 ++ es32).size))
        }
      }
      
      it("should return a one tree for three connected components") {
        forAll(genUnwDiGraphParamData3) {
          case ((GraphParamData(ps1, vs1, es1), v1), (GraphParamData(ps2, vs2, es2), v2), (GraphParamData(ps3, vs3, es3), v3)) => 
            val g = graphFactory() ++ ps1 ++ ps2 ++ ps3
            val t = g.dfsFrom(v1)
            t.vertices.toSet should be === vs1
            t.vertices should have size(vs1.size)
        }
      }
    }
    
    describe("dfsFromStack") {
      it("should return a tree with the first vertices from the start vertex and the second vertices from the first vertices") {
        forAll(genUnwDiEdges123) {
          case Edges123(v, es1, es12, _, _, _, _, _) =>
            val g = (graphFactory() ++ es1 ++ es12)
            val ts = g.dfsFromStack(Seq(v))
            ts.keySet should be(Set(v))
            ts(v).childEdgesFrom(v).toSet should be ===(es1.toSet)
            ts(v).childEdgesFrom(v) should have size(es1.size)
            es1.flatMap { e => ts(v).edgesFrom(e.out) }.toSet should be ===(es12.toSet)
            es1.flatMap { e => ts(v).edgesFrom(e.out) }.toSet should have size(es12.size)
        }
      }

      it("should return a tree with two branches for the triangle graph") {
        forAll(genUnwDiTriangleEdges) {
          case TriangleEdges(v, es12, es13, es23, es32) =>
            val g = graphFactory() ++ es12 ++ es13 ++ es23 ++ es32
            val ts = g.dfsFromStack(Seq(v))
            ts.keySet should be ===(Set(v))
            ts(v).edges.toSet should(be ===((es12 ++ es13.init ++ es23).toSet) or be ===((es12.init ++ es13 ++ es32).toSet))
            ts(v).edges should(have size((es12 ++ es13.init ++ es23).size) or have size((es12.init ++ es13 ++ es32).size))
        }
      }

      it("should return three trees for three connected components") {
        forAll(genUnwDiGraphParamData3) {
          case ((GraphParamData(ps1, vs1, es1), v1), (GraphParamData(ps2, vs2, es2), v2), (GraphParamData(ps3, vs3, es3), v3)) => 
          	val g = graphFactory() ++ ps1 ++ ps2 ++ ps3
          	val ts = g.dfsFromStack(Seq(v1, v2, v3))
          	ts.keySet should be ===(Set(v1, v2, v3))
          	ts(v1).vertices.toSet should be === vs1
          	ts(v1).vertices should have size(vs1.size)
          	ts(v2).vertices.toSet should be === vs2
          	ts(v2).vertices should have size(vs2.size)
          	ts(v3).vertices.toSet should be === vs3
          	ts(v3).vertices should have size(vs3.size)
        }
      }
      
      it("should return one tree and two trees for one connected componet") {
        forAll(for(d3 <- genUnwDiGraphParamData3; u2 <- Gen.oneOf(d3._1._1.vs.toSeq); u3 <- Gen.oneOf((d3._1._1.vs - u2).toSeq)) yield (d3, u2, u3)) {
          case (((GraphParamData(ps1, vs1, es1), v1), (GraphParamData(ps2, vs2, es2), v2), (GraphParamData(ps3, vs3, es3), v3)), u2, u3) => 
          	val ps = (ps1 ++ ps2 ++ ps3) + (u2 -> v2 unw) + (u3 -> v3 unw)
            val g = graphFactory() ++ ps
          	val ts = g.dfsFromStack(Seq(v3, v2, v1))
          	ts.keySet should be ===(Set(v1, v2, v3))
          	ts(v1).vertices.toSet should be === (vs1) 
          	ts(v1).vertices should have size(vs1.size)
          	ts(v2).vertices.toSet should be === vs2
          	ts(v2).vertices should have size(vs2.size)
          	ts(v3).vertices.toSet should be === vs3
          	ts(v3).vertices should have size(vs3.size)
        }
      }
    }
  }
  
  def bfs
  {
    describe("bfsFrom") {
      it("should return a tree with the first vertices from the start vertex and the second vertices from the first vertices") {
        forAll(genUnwDiEdges123) {
          case Edges123(v, es1, es12, es10, es11, es20, es21, es22) =>
            //println(Edges123(v, es1, es12, es10, es11, es20, es21, es22))
            val g = (graphFactory() ++ es1 ++ es12 ++ es10 ++ es11 ++ es20 ++ es21 ++ es22)
            val t = g.bfsFrom(v)
            t.childEdgesFrom(v).toSet should be ===(es1.toSet)
            t.childEdgesFrom(v) should have size(es1.size)
            es1.flatMap { e => t.childEdgesFrom(e.out) }.toSet should be ===(es12.toSet)
            es1.flatMap { e => t.childEdgesFrom(e.out) }.toSet should have size(es12.size)
        }
      }
      
      it("should return a one tree for three connected components") {
        forAll(genUnwDiGraphParamData3) {
          case ((GraphParamData(ps1, vs1, es1), v1), (GraphParamData(ps2, vs2, es2), v2), (GraphParamData(ps3, vs3, es3), v3)) => 
            val g = (graphFactory() ++ ps1 ++ ps2 ++ ps3)
            val t = g.bfsFrom(v1)
            //println(t)
            t.vertices.toSet should be ===(vs1)
            t.vertices should have size(vs1.size)
        }
      }
    } 
    
    describe("bfsFromQueue") {
      it("should return a tree with the first vertices from the start vertex and the second vertices from the first vertices") {
        forAll(genUnwDiEdges123) {
          case Edges123(v, es1, es12, es10, es11, es20, es21, es22) =>
            //println(Edges123(v, es1, es12, es10, es11, es20, es21, es22))
            val g = (graphFactory() ++ es1 ++ es12 ++ es10 ++ es11 ++ es20 ++ es21 ++ es22)
            val ts = g.bfsFromQueue(Seq(v))
            ts.keySet should be(Set(v))
            ts(v).childEdgesFrom(v).toSet should be ===(es1.toSet)
            ts(v).childEdgesFrom(v) should have size(es1.size)
            es1.flatMap { e => ts(v).edgesFrom(e.out) }.toSet should be ===(es12.toSet)
            es1.flatMap { e => ts(v).edgesFrom(e.out) }.toSet should have size(es12.size)
        }
      }
      
      it("should return three trees for three connected components") {
        forAll(genUnwDiGraphParamData3) {
          case ((GraphParamData(ps1, vs1, es1), v1), (GraphParamData(ps2, vs2, es2), v2), (GraphParamData(ps3, vs3, es3), v3)) =>
            val g = (graphFactory() ++ ps1 ++ ps2 ++ ps3)
            val ts = g.bfsFromQueue(Seq(v1, v2, v3))
            //println(ts)
            ts should have size(3)
            ts.keySet should be === Set(v1, v2, v3)
            ts(v1).vertices.toSet should be === vs1
            ts(v1).vertices should have size(vs1.size)
            ts(v2).vertices.toSet should be === vs2
            ts(v2).vertices should have size(vs2.size)
            ts(v3).vertices.toSet should be === vs3
            ts(v3).vertices should have size(vs3.size)
        }
      }
      
      it("should return a tree with 2 depth and a tree with 1 depth for the diamond graph") {
        forAll(genUnwDiDiamondEdges) {
          case DiamondEdges(v1, es12, es23, es43, es54, v5) =>
            //println(DiamondEdges(v1, es12, es23, es43, es54, v5))
            val g = graphFactory() ++ es12 ++ es23 ++ es43 ++ es54
            val ts = g.bfsFromQueue(Seq(v1, v5))
            ts.keySet should be === Set(v1, v5)
            ts(v1).edgesFrom(v1).toSet should be ===(es12.toSet)
            ts(v1).edgesFrom(v1) should have size(es12.size)
            es12.flatMap { e => ts(v1).edgesFrom(e.out) }.toSet should be ===(es23.toSet)
            es12.flatMap { e => ts(v1).edgesFrom(e.out) } should have size(es23.size)
            ts(v5).edgesFrom(v5).toSet should be ===(es54.toSet)
            ts(v5).edgesFrom(v5) should have size(es54.size)
        }
      }
    }
  }
}
