package org.lkbgraph.spec
import org.scalacheck._
import org.lkbgraph._

object GraphParamGen 
{
  val genVertices = Gen.containerOf[Set, Char](Gen.alphaChar)
  
  object GraphGen
  {
    def genUnwDiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, UnwDiEdge[Char]](Gen.pick(2, vs).map { case Seq(v, u) => UnwDiEdge[Char](v, u) })

    def genUnwUndiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, UnwUndiEdge[Char]](Gen.pick(2, vs).map { case Seq(v, u) => UnwUndiEdge[Char](v, u) })
  
    def genWDiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, WDiEdge[Char, Int]](Gen.pick(2, vs).map2(Gen.choose(-1000, 1000)) { 
        case (Seq(v, u), w) => WDiEdge[Char, Int](v, u, w) 
      }).map { es => es.map { e => (e.toUnweightedEdge, e) }.toMap.values }
  
    def genWUndiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, WUndiEdge[Char, Int]](Gen.pick(2, vs).map2(Gen.choose(-1000, 1000)) { 
        case (Seq(v, u), w) => WUndiEdge[Char, Int](v, u, w) 
      }).map { es => es.map { e => (e.toUnweightedEdge, e) }.toMap.values }
  
    val genUnwDiGraphParams = for(vs <- genVertices; es <- genUnwDiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }
  
    val genUnwUndiGraphParams = for(vs <- genVertices; es <- genUnwUndiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }

    val genWDiGraphParams = for(vs <- genVertices; es <- genWDiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }

    val genWUndiGraphParams = for(vs <- genVertices; es <- genWUndiEdges(vs)) yield { vs.map(Vertex[Char]) ++ es }

    val genUnwDiGraphParamsAndExpectData: Gen[(Set[GraphParam[Char, Unweighted, DiEdge]], Set[Char], Set[UnwDiEdge[Char]])] = 
      for(vs <- genVertices; es <- genUnwDiEdges(vs)) yield { (vs.map(Vertex[Char]) ++ es, vs, es) }
  
    val genUnwUndiGraphParamsAndExpectData = for(vs <- genVertices; es <- genUnwUndiEdges(vs)) yield { (vs.map(Vertex[Char]) ++ es, vs, es) }

    val genWDiGraphParamsAndExpectData = for(vs <- genVertices; es <- genWDiEdges(vs)) yield { (vs.map(Vertex[Char]) ++ es, vs, es) }

    val genWUndiGraphParamsAndExpectData = for(vs <- genVertices; es <- genWUndiEdges(vs)) yield { (vs.map(Vertex[Char]) ++ es, vs, es) }
  }
}