package ltq.digraph.dijkstra

import ltq.digraph.Digraph.Weight
import ltq.digraph.dijkstra.DijkstraAlgorithmWithPriorityQueue.PriorityQueueStorage
import ltq.digraph.{Digraph, ShortestPathTree}

import scala.collection.mutable


case class DijkstraAlgorithmWithPriorityQueue[T](override val root: T, g: Digraph[T])
    extends ShortestPathTree[T] with DijkstraAlgorithm[T] {

  override protected val graph: T => Map[T, Weight] = { vertex =>
    g.adjacent(vertex).map(edge => (edge.end, edge.weight)).toMap
  }

  override def distanceTo(vertex: T): Option[Weight] = {
    distances.get(vertex)
  }

  override def predecessorOf(vertex: T): Option[T] = {
    this.predecessors.get(vertex)
  }

  override protected def initStorage(root: T): ProcessStorage[T] = {
    implicit val ordering: Ordering[(T, Weight)] = Ordering.fromLessThan{case ((_, w1), (_, w2)) => w1 > w2}
    PriorityQueueStorage(mutable.PriorityQueue(root -> 0))
  }
}

object DijkstraAlgorithmWithPriorityQueue {

  case class PriorityQueueStorage[T](values: mutable.PriorityQueue[(T, Weight)]) extends ProcessStorage[T] {

    override def head: T =  values.head._1

    override def tail: ProcessStorage[T] = PriorityQueueStorage(values.tail)

    override def add(xs: Iterable[(T, Weight)]): ProcessStorage[T] ={
      PriorityQueueStorage(values ++ xs)
    }

    override def isEmpty: Boolean = values.isEmpty
  }

}

