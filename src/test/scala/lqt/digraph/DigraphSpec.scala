package lqt.digraph

import ltq.digraph.Digraph
import ltq.digraph.Digraph.Edge
import org.scalatest.wordspec.AnyWordSpec

class DigraphSpec extends AnyWordSpec {

  "adding duplicated Edge" in {
    val graph = Digraph[String](Set(Edge("WLD", "CDP", 1000)))
    assert(graph.addEdge(Edge("WLD", "CDP", 834)).edges.size == 1)
  }

}
