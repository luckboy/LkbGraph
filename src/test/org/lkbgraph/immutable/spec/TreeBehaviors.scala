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

  trait AddEdgeGens[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]
  {
    def treePrefix: String
      
    def makeEdge(v: Char, u: Char): E[Char, X]
    
    def genEdges(root: Char, vs: Set[Char]): Gen[Seq[E[Char, X]]]
    
    def genTreeParamData: Gen[TreeParamData[Char, E[Char, X]]]
    
    val genTreeParamDataWithoutEdge = for { 
      vs <- genVertices
      us <- Gen.pick(2, vs)
      v <- Gen.oneOf((vs -- us).toSeq)
      es <- genEdges(v, (vs -- us)) 
    } yield {
      val Seq(u1, u2) = us
      (TreeParamData(v, vs -- us, es.toSet), makeEdge(u1, u2))
    }

    val genTreeParamDataWithoutEdgeThatHasInputVertexIsExists = for { 
      vs <- genVertices
      u1 <- Gen.oneOf(vs.toSeq)
      u2 <- Gen.oneOf((vs - u1).toSeq)
      v <- Gen.oneOf((vs - u2).toSeq)
      es <- genEdges(v, (vs - u2)) 
    } yield {
      (TreeParamData(v, vs - u2, es.toSet), makeEdge(u1, u2))
    }

    val genTreeParamDataWithoutEdgeThatHasOutputVertexIsExists = for { 
      vs <- genVertices
      u1 <- Gen.oneOf(vs.toSeq)
      u2 <- Gen.oneOf((vs - u1).toSeq)
      v <- Gen.oneOf((vs - u1).toSeq)
      es <- genEdges(v, (vs - u1)) 
    } yield {
      (TreeParamData(v, vs - u1, es.toSet), makeEdge(u1, u2))
    }
  }
  
  implicit object UnwDiAddEdgeGens extends AddEdgeGens[Unweighted, DiEdge]
  {
    override def treePrefix = "directed"
      
    override def makeEdge(v: Char, u: Char) = v -> u
    
    override def genEdges(root: Char, vs: Set[Char]) = genUnwDiEdges(root, vs)
    
    override def genTreeParamData = genUnwDiTreeParamData
  }

  implicit object UnwUndiAddEdgeGens extends AddEdgeGens[Unweighted, UndiEdge]
  {
    override def treePrefix = "undirected"
      
    override def makeEdge(v: Char, u: Char) = v ~ u
    
    override def genEdges(root: Char, vs: Set[Char]) = genUnwUndiEdges(root, vs)
    
    override def genTreeParamData = genUnwUndiTreeParamData
  }
  
  def genUnwDiTreeParamDataWithoutEdgeThatHasOutputVertexIsExists = 
    UnwDiAddEdgeGens.genTreeParamDataWithoutEdgeThatHasOutputVertexIsExists

  def genUnwUndiTreeParamDataWithoutEdgeThatHasOutputVertexIsExists = 
    UnwUndiAddEdgeGens.genTreeParamDataWithoutEdgeThatHasOutputVertexIsExists
  
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
      def edgeAddable[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: AddEdgeGens[X, E]) = {
        import gens._

        it("should return a copy of the " + treePrefix + " tree without a new edge that has both vertices which are non-existent") {
          forAll(genTreeParamDataWithoutEdge) {
            case (TreeParamData(v, vs, es), e) =>
              val t = (treeFactory.newTreeBuilder(v) ++= es).result
              val t2 = t +~^ e
              t2.vertices.toSet should be ===(vs)
              t2.vertices should have size(vs.size)
              t2.edges.toSet should be ===(es)
              t2.edges should have size(es.size)
          }
        }

        it("should return a copy of the " + treePrefix + " tree with a new edge that has the input vertex that is existent") {
          forAll(genTreeParamDataWithoutEdgeThatHasInputVertexIsExists) {
            case (TreeParamData(v, vs, es), e) =>
              val t = (treeFactory.newTreeBuilder(v) ++= es).result
              val t2 = t +~^ e
              t2.vertices.toSet should be ===(vs + e.out)
              t2.vertices should have size(vs.size + 1)
              t2.edges.toSet should be ===(es + e)
              t2.edges should have size(es.size + 1)
          }
        }

        it("should return a copy of the " + treePrefix + " tree without a new edge that has both vertices which are existent") {
          forAll(for(d <- genTreeParamData; e <- Gen.pick(2, d.vs).map { case Seq(v, u) => makeEdge(v, u) }) yield (d, e)) {
            case (TreeParamData(v, vs, es), e) =>
              val t = (treeFactory.newTreeBuilder(v) ++= es).result
              val t2 = t +~^ e
              t2.vertices.toSet should be ===(vs)
              t2.vertices should have size(vs.size)
              t2.edges.toSet should be ===(es)
              t2.edges should have size(es.size)
          }
        }
      }

      it should behave like edgeAddable[Unweighted, DiEdge]
      it should behave like edgeAddable[Unweighted, UndiEdge]

      it("should return a copy of the directed tree without a new edge that has the output vertex that is existent") {
        forAll(genUnwDiTreeParamDataWithoutEdgeThatHasOutputVertexIsExists) {
          case (TreeParamData(v, vs, es), e) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            val t2 = t +~^ e
            t2.vertices.toSet should be ===(vs)
            t2.vertices should have size(vs.size)
            t2.edges.toSet should be ===(es)
            t2.edges should have size(es.size)
        }
      }

      it("should return a copy of the undirected tree with a new edge that has the output vertex that is existent") {
        forAll(genUnwUndiTreeParamDataWithoutEdgeThatHasOutputVertexIsExists) {
          case (TreeParamData(v, vs, es), e) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            val t2 = t +~^ e
            t2.vertices.toSet should be ===(vs + e.in)
            t2.vertices should have size(vs.size + 1)
            t2.edges.toSet should be ===(es + e)
            t2.edges should have size(es.size + 1)
        }
      }

      it("should return a directed tree with the replaced edge with weight for a new weight") {
        forAll(for(t <- genWDiTreeParamData; e <- Gen.oneOf(t.es.toSeq); dw <- Gen.choose(1, 1000)) yield (t, e, dw)) {
          case (TreeParamData(v, vs, es), e, dw) => 
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            val e2 = e.in -> e.out w (e.weight ^ dw)
            val t2 = t +~^ e2
            t2.vertices.toSet should be ===(vs)
            t2.vertices should have size(vs.size)
            t2.edges.toSet should be ===((es - e) + e2)
            t2.edges should have size(es.size)
        }
      }

      it("should return a undirected tree with the replaced edge with weight for a new weight") {
        forAll(for(t <- genWUndiTreeParamData; e <- Gen.oneOf(t.es.toSeq); dw <- Gen.choose(1, 1000)) yield (t, e, dw)) {
          case (TreeParamData(v, vs, es), e, dw) => 
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            val e2 = e.in ~ e.out w (e.weight ^ dw)
            val t2 = t +~^ e2
            t2.vertices.toSet should be ===(vs)
            t2.vertices should have size(vs.size)
            t2.edges.toSet should be ===((es - e) + e2)
            t2.edges should have size(es.size)
        }
      }

      it("should return a undirected tree with the replaced swaped edge with weight for a new weight") {
        forAll(for(t <- genWUndiTreeParamData; e <- Gen.oneOf(t.es.toSeq); dw <- Gen.choose(1, 1000)) yield (t, e, dw)) {
          case (TreeParamData(v, vs, es), e, dw) => 
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            val e2 = e.out ~ e.in w (e.weight ^ dw)
            val t2 = t +~^ e2
            t2.vertices.toSet should be ===(vs)
            t2.vertices should have size(vs.size)
            t2.edges.toSet should be ===((es - e) + e2)
            t2.edges should have size(es.size)
        }
      }
    }
    
    describe("-@^") {
      it("should return a copy of the tree without the unmodified edges and vertices for a non-existent node") {
        forAll(for {
            vs <- genVertices
            v <- Gen.oneOf(vs.toSeq)
            u <- Gen.oneOf((vs - v).toSeq)
            es <- genUnwDiEdges(v, vs - u)
          } yield (TreeParamData(v, vs - u, es.toSet), u)) {
          case (TreeParamData(v, vs, es), u) =>
            val t = (treeFactory.newTreeBuilder(v) ++= es).result
            val t2 = t -@^ u
            t2.vertices.toSet should be ===(vs)
            t2.vertices should have size(vs.size)
            t2.edges.toSet should be ===(es)
            t2.edges should have size(es.size)
        }
      }
      
      it("should return a copy of the tree without the branch for the edge") {
        forAll(for {
          vs <- genVertices
          (vs1, vs2) <- Gen.someOf(vs.toSeq).map { us => ((vs -- us).toSet,  us.toSet) }
          v1 <- Gen.oneOf(vs1.toSeq)
          v2 <- Gen.oneOf(vs2.toSeq)
          es1 <- genUnwDiEdges(v1, vs1)
          es2 <- genUnwDiEdges(v2, vs2)
          u <- Gen.oneOf(vs1.toSeq)
        } yield {
          (TreeParamData(v1, vs1, es1.toSet), TreeParamData(v2, vs2, es2.toSet), u -> v2 unw) 
        }) {
          case (TreeParamData(v1, vs1, es1), TreeParamData(v2, vs2, es2), e) => 
            val t = (treeFactory.newTreeBuilder(v1) ++= (es1 ++ es2 + e)).result
            val t2 = t -@^ e.out
            t2.vertices.toSet should be ===(vs1 + e.out)
            t2.vertices should have size(vs1.size + 1)
            t2.edges.toSet should be ===(es1 + e)
            t2.edges should have size(es1.size + 1)
        }
      }
    }
    
    it should behave like treeTraversal
  }

  case class TraversalData[V, E](v: V, vs1: Seq[V], vss2: Seq[Seq[V]], es1: Seq[E], ess2: Seq[Seq[E]])

  trait TraversalGens[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]]  
  {
    def treePrefix: String
    
    def makeEdge(v: VertexType, u: VertexType): E[VertexType, X]
    
    def genTraversalData: Gen[TraversalData[VertexType, E[VertexType, X]]] = for {
      vs <- genVertices
      v0 <- Gen.oneOf(vs.toSeq)
      
      vs1 <- Gen.pick(4, vs - v0)
      
      vs2 <- Gen.pick(3, vs -- vs1 - v0)
      vs3 <- Gen.pick(4, vs -- vs1 -- vs2 - v0)
      vs4 <- Gen.pick(3, vs -- vs1 -- vs2 -- vs3 - v0)
    } yield {
      val es1 = vs1.map { makeEdge(v0, _) }
      val es2 = vs2.map { makeEdge(vs1(0), _) }
      val es3 = vs3.map { makeEdge(vs1(2), _) }
      val es4 = vs4.map { makeEdge(vs1(3), _) }
      TraversalData(v0, vs1, Seq(vs2, Seq(), vs3, vs4), es1, Seq(es2, Seq(), es3, es4))
    }
  }
  
  implicit object UnwDiTraversalGens extends TraversalGens[Unweighted, DiEdge]
  {
    override def treePrefix = "directed"
    
    override def makeEdge(v: VertexType, u: VertexType) = v -> u unw
  }

  implicit object UnwUndiTraversalGens extends TraversalGens[Unweighted, UndiEdge]
  {
    override def treePrefix = "undirected"
    
    override def makeEdge(v: VertexType, u: VertexType) = v ~ u
  }
  
  def treeTraversal
  { 
    describe("preOrderFrom") {
      def traversal[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: TraversalGens[X, E]) = {
        import gens._
        
        it("should return the pre-order traversal sequence for the root and " + treePrefix + " tree") {
          forAll(genTraversalData) {
            case TraversalData(v0, vs1, vss2, es1, ess2) =>
              val t = (treeFactory.newTreeBuilder(v0) ++= es1 ++ ess2.flatten).result
              val seq = t.preOrderFrom(v0)
              //println(seq)
              seq.first should be ===(v0)
              seq should have size(seq.toSeq.size)
              for((u, us) <- vs1.zip(vss2)) {
                seq.indexOf(u) should be >=(1)
                if(!us.isEmpty) {
                  //println(u)
                  //println(us.map { seq.indexOf(_) })
                  seq.indexOf(u) should be ===(us.map { seq.indexOf(_) }.min - 1)
                  us.map { seq.indexOf(_) }.toSet should be ===((us.map { seq.indexOf(_) }.min to us.map { seq.indexOf(_) }.max).toSet)
                }
              }
          }
        }
      }
      
      it should behave like traversal[Unweighted, DiEdge]
      it should behave like traversal[Unweighted, UndiEdge]
    }
    
    describe("postOrderFrom") {
      def traversal[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: TraversalGens[X, E]) = {
        import gens._
        
        it("should return the post-order traversal sequence for the root and " + treePrefix + " tree") {
          forAll(genTraversalData) {
            case TraversalData(v0, vs1, vss2, es1, ess2) =>
              val t = (treeFactory.newTreeBuilder(v0) ++= es1 ++ ess2.flatten).result
              val seq = t.postOrderFrom(v0)
              //println(seq)
              seq.last should be ===(v0)
              seq should have size(seq.toSeq.size)
              for((u, us) <- vs1.zip(vss2)) {
                seq.indexOf(u) should be >=(1)
                if(!us.isEmpty) {
                  //println(u)
                  //println(us.map { seq.indexOf(_) })
                  seq.indexOf(u) should be ===(us.map { seq.indexOf(_) }.max + 1)
                  us.map { seq.indexOf(_) }.toSet should be ===((us.map { seq.indexOf(_) }.min to us.map { seq.indexOf(_) }.max).toSet)
                }
              }
          }
        }
      }

      it should behave like traversal[Unweighted, DiEdge]
      it should behave like traversal[Unweighted, UndiEdge]
    }
    
    describe("levelOrderFrom") {
      def traversal[X, E[+Y, +Z] <: EdgeLike[Y, Z, E]](implicit gens: TraversalGens[X, E]) = {
        import gens._
        
        it("should return the level-order traversal sequence for the root and " + treePrefix + " tree") {
          forAll(genTraversalData) {
            case TraversalData(v0, vs1, vss2, es1, ess2) =>
              val t = (treeFactory.newTreeBuilder(v0) ++= es1 ++ ess2.flatten).result
              val seq = t.levelOrderFrom(v0)
              seq.first should be ===(v0)
              (0 until vs1.size).map { i => seq(i + 1) }.toSet should be ===(vs1.toSet)
              (0 until vss2.flatten.size).map { i => seq(i + vs1.size + 1) }.toSet should be ===(vss2.flatten.toSet)
          }
        }
      }

      it should behave like traversal[Unweighted, DiEdge]
      it should behave like traversal[Unweighted, UndiEdge]
    }
  }
}
