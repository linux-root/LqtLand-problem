package lqt.digraph

import ltq.digraph.Digraph.Edge
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

}
