package lqt.digraph.dijkstra

import lqt.digraph.ShortestPathTreeBenchmark
import ltq.digraph.dijkstra.{DijkstraAlgorithmWithPriorityQueue, DijkstraAlgorithmWithSortedSet, DijkstraAlgorithmWithUnorderedStorage}
import ltq.digraph.{Digraph, ShortestPathTree}
import org.scalacheck.Gen

class DijkstraAlgorithmWithPriorityQueueBenchmark extends ShortestPathTreeBenchmark[String]{
  override protected def algorithmName: String = "Dijkstra with Priority Queue"

  override protected def genValue: Gen[String] = {
    Gen.listOfN(10, Gen.alphaUpperChar).map(_.mkString)
  }

  override protected def createTree(root: String, graph: Digraph[String]): ShortestPathTree[String] = {
    DijkstraAlgorithmWithPriorityQueue(root, graph)
  }

  override protected val expectedMaxExecutionTimeInMillis: Long = 60000
}