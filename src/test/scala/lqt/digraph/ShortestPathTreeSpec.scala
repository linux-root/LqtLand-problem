package lqt.digraph

import ltq.digraph.Digraph.Edge
import ltq.digraph.dijkstra.DijkstraAlgorithmWithUnorderedStorage
import ltq.digraph.{Digraph, ShortestPathTree}
import org.scalatest.wordspec.AnyWordSpec

class ShortestPathTreeSpec extends AnyWordSpec {

  "Failed test case found by ShortestPathTreePropertySpec" in {
    val input = """(MNQWD)--27-->(MGDVK) (JEDPR)--13-->(MNQWD) (MNQWD)--6-->(JIGYB)
     (MNQWD)--48-->(MACHD) (JEDPR)--50-->(JIGYB) (DDIUO)--1-->(MNQWD)
     (JIGYB)--4-->(MNQWD) (MNQWD)--14-->(BAAHE) (JIGYB)--5-->(MACHD) (JEDPR)--18-->(JIGYB)"""

    val edges = Edge.parseStrEdges(input)
    val graph = Digraph(edges.toSet)
    graph.vertices.foreach { vertex =>
      val tree = ShortestPathTree(vertex, graph)
      assert(ShortestPathTreePropertySpec.checkOptimalityCondition(tree, graph))
    }
  }

    "example test" in {
      val input ="""(JMA)--1029-->(LCY) (QTE)--1-->(YUO) (JMA)--327-->(YUO) (JMA)--1-->(PQG)
           (KHD)--10000-->(PQG) (XGA)--1577-->(XSU) (LCY)--3322-->(YUO) (JMA)--10000-->(XSU)
           (IYP)--3230-->(XGA) (PQG)--1-->(JMA) (LCY)--1-->(JMA) (YUO)--1-->(XSU) (EYL)--10000-->(JMA)
           (XSU)--8363-->(PQG) (PQG)--10000-->(XSU) (LCY)--5739-->(PQG) (KHD)--9449-->(LCY) (PQG)--1-->(KHD)
           (JMA)--1-->(EYL) (KHD)--253-->(EYL) (LCY)--10000-->(EYL) (JMA)--1-->(KHD) (XSU)--451-->(YUO)
           (YUO)--1-->(JMA) (LCY)--593-->(XSU) (XSU)--10000-->(JMA) (EYL)--1-->(PQG) (PQG)--1-->(EYL)
           (YUO)--88-->(LCY) (KHD)--2996-->(JMA) (XSU)--1-->(LCY)"""

      val root = "JMA"
      val edges = Edge.parseStrEdges(input)
      val graph = Digraph(edges.toSet)
      val spt = DijkstraAlgorithmWithUnorderedStorage(root, graph)

      spt.predecessorOf(root)

  }

}
