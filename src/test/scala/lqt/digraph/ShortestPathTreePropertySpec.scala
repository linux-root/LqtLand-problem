package lqt.digraph

import ltq.digraph.{Digraph, ShortestPathTree}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object ShortestPathTreePropertySpec extends Properties("ShortestPathTree") with DigraphPropertyGeneralSpec {
  implicit val genStringVertices: Gen[String] = Gen.listOfN(5, Gen.alphaUpperChar).map(_.mkString)

  property(
    """Optimality condition:
      given r is the root of shortest path tree.
      For each vertex x, distanceTo(x) is the distance from r to x
      For each edge e = v -> w, the following inequality must hold:
              distanceTo(w) <= distanceTo(v) +  e.weight
      """.stripMargin) = forAll(genConnectedDigraph[String]) { graph =>
    graph.vertices.map(v => ShortestPathTree(v, graph)).forall(checkOptimalityCondition(_, graph))
  }

  def checkOptimalityCondition[T](tree: ShortestPathTree[T], graph: Digraph[T]): Boolean = {
    graph.edges.forall { edge =>
      tree.distanceTo(edge.start).map(_ + edge.weight).forall { dis =>
        tree.distanceTo(edge.end).get <= dis
      }
    }
  }

}
