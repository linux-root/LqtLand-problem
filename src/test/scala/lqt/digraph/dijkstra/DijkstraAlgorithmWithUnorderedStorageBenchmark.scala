package lqt.digraph.dijkstra

import lqt.digraph.ShortestPathTreeBenchmark
import ltq.digraph.dijkstra.DijkstraAlgorithmWithUnorderedStorage
import ltq.digraph.{Digraph, ShortestPathTree}
import org.scalacheck.Gen

class DijkstraAlgorithmWithUnorderedStorageBenchmark extends ShortestPathTreeBenchmark[String]{
  override protected def algorithmName: String = "Dijkstra with Unordered storage"

  override protected def genValue: Gen[String] = {
    Gen.listOfN(10, Gen.alphaUpperChar).map(_.mkString)
  }


  override protected def createTree(root: String, graph: Digraph[String]): ShortestPathTree[String] = {
    DijkstraAlgorithmWithUnorderedStorage(root, graph)
  }

  override protected val expectedMaxExecutionTimeInMillis: Long = 30000
}
