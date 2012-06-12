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
      Gen.containerOf[Set, DiEdge[Char, Unweighted]](Gen.pick(2, vs).map { case Seq(v, u) => UnwDiEdge[Char](v, u) })

    def genUnwUndiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, UndiEdge[Char, Unweighted]](Gen.pick(2, vs).map { case Seq(v, u) => UnwUndiEdge[Char](v, u) })
  
    def genWDiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, DiEdge[Char, Weighted[Int]]](Gen.pick(2, vs).map2(arbitrary[Int]) { 
        case (Seq(v, u), w) => WDiEdge[Char, Int](v, u, w) 
      }).map { es => es.map { e => (e.toUnweightedEdge, e) }.toMap.values.toSet }
  
    def genWUndiEdges(vs: Set[Char]) = 
      Gen.containerOf[Set, UndiEdge[Char, Weighted[Int]]](Gen.pick(2, vs).map2(arbitrary[Int]) { 
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
      GraphParamData[GraphParam[Char, Unweighted, UndiEdge], Char, UndiEdge[Char, Unweighted]](vs.map(Vertex[Char]) ++ es, vs, es) 
    }

    val genWDiGraphParamData = for(vs <- genVertices; es <- genWDiEdges(vs)) yield { 
      GraphParamData(vs.map(Vertex[Char]) ++ es, vs, es) 
    }

    val genWUndiGraphParamData = for(vs <- genVertices; es <- genWUndiEdges(vs)) yield { 
      GraphParamData(vs.map(Vertex[Char]) ++ es, vs, es)
    }
  }
}
