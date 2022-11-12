package ltq.digraph

import ltq.digraph.Digraph.Weight
import ltq.digraph.dijkstra.DijkstraAlgorithmWithPriorityQueue


trait ShortestPathTree[T] {
  def root: T

  def distanceTo(vertex: T): Option[Weight]

  def predecessorOf(vertex: T): Option[T]
}

object ShortestPathTree {
  def apply[T](v: T, g: Digraph[T]): ShortestPathTree[T] = DijkstraAlgorithmWithPriorityQueue(v, g)
}