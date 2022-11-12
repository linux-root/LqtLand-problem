package ltq.digraph.dijkstra

import ltq.digraph.Digraph.Weight
import ltq.digraph.dijkstra.DijkstraAlgorithmWithUnorderedStorage.UnorderedStorage
import ltq.digraph.{Digraph, ShortestPathTree}


case class DijkstraAlgorithmWithUnorderedStorage[T](override val root: T, g: Digraph[T])
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

  override protected def initStorage(root: T): ProcessStorage[T] = UnorderedStorage(Map(root -> 0))
}

object DijkstraAlgorithmWithUnorderedStorage {

  case class UnorderedStorage[T](values: Map[T, Weight]) extends ProcessStorage[T] {

    override def head: T =  values.head._1

    override def tail: ProcessStorage[T] = UnorderedStorage(values - head)

    override def add(xs: Iterable[(T, Weight)]): ProcessStorage[T] = UnorderedStorage(values ++ xs)

    override def isEmpty: Boolean = values.isEmpty
  }

}

