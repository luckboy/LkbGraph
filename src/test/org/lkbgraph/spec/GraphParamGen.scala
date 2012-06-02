package org.lkbgraph.spec
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.lkbgraph._

object GraphParamGen 
{
  val genVertices = Gen.containerOf[Set, Char](Gen.alphaChar)
  
  case class GraphParamData[GP, V, E](ps: Set[GP], vs: Set[V], es: Set[E])
  
  object GraphGen
  {
    def genUnwDiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, UnwDiEdge[Char]](Gen.pick(2, vs).map { case Seq(v, u) => UnwDiEdge[Char](v, u) })

    def genUnwUndiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, UnwUndiEdge[Char]](Gen.pick(2, vs).map { case Seq(v, u) => UnwUndiEdge[Char](v, u) })
  
    def genWDiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, WDiEdge[Char, Int]](Gen.pick(2, vs).map2(arbitrary[Int]) { 
        case (Seq(v, u), w) => WDiEdge[Char, Int](v, u, w) 
      }).map { es => es.map { e => (e.toUnweightedEdge, e) }.toMap.values.toSet }
  
    def genWUndiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, WUndiEdge[Char, Int]](Gen.pick(2, vs).map2(arbitrary[Int]) { 
        case (Seq(v, u), w) => WUndiEdge[Char, Int](v, u, w) 
      }).map { es => es.map { e => (e.toUnweightedEdge, e) }.toMap.values.toSet }
  
    val genUnwDiGraphParams = for(vs <- genVertices; es <- genUnwDiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }
  
    val genUnwUndiGraphParams = for(vs <- genVertices; es <- genUnwUndiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }

    val genWDiGraphParams = for(vs <- genVertices; es <- genWDiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }

    val genWUndiGraphParams = for(vs <- genVertices; es <- genWUndiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }

    val genUnwDiGraphParamData = for(vs <- genVertices; es <- genUnwDiEdges(vs)) yield { 
      GraphParamData(vs.map(Vertex[Char]) ++ es, vs, es) 
    }
  
    val genUnwUndiGraphParamData = for(vs <- genVertices; es <- genUnwUndiEdges(vs)) yield { 
      GraphParamData(vs.map(Vertex[Char]) ++ es, vs, es) 
    }

    val genWDiGraphParamData = for(vs <- genVertices; es <- genWDiEdges(vs)) yield { 
      GraphParamData(vs.map(Vertex[Char]) ++ es, vs, es) 
    }

    val genWUndiGraphParamData = for(vs <- genVertices; es <- genWUndiEdges(vs)) yield { 
      GraphParamData(vs.map(Vertex[Char]) ++ es, vs, es)
    }
  }
}