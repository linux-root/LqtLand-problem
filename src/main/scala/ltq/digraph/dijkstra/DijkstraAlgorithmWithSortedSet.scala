package ltq.digraph.dijkstra

import ltq.digraph.Digraph.Weight
import ltq.digraph.dijkstra.DijkstraAlgorithmWithSortedSet.{Distance, Storage}
import ltq.digraph.{Digraph, ShortestPathTree}

import scala.collection.SortedSet


case class DijkstraAlgorithmWithSortedSet[T](override val root: T, g: Digraph[T])
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
    implicit val ordering: Ordering[Distance[T]] = Ordering.by(_.weight)
    Storage(SortedSet(Distance(root, 0)))
  }
}

object DijkstraAlgorithmWithSortedSet {

  case class Distance[T](vertex: T, weight: Weight) {
    override def equals(obj: Any): Boolean = obj match {
      case d : Distance[?] => this.vertex == d.vertex
      case _ => false
    }

    override def hashCode(): Weight = {
      this.vertex.hashCode()
    }
  }

  private case class Storage[T](vertices: SortedSet[Distance[T]]) extends ProcessStorage[T] {

    override def head: T = vertices.head.vertex

    override def tail: ProcessStorage[T] = Storage(vertices.tail)

    override def add(xs: Iterable[(T, Weight)]): ProcessStorage[T] = {
      val distances =  xs.map{case (v, weight) => Distance(v, weight)}.toSet
      Storage(this.vertices.filterNot(distances).concat(distances))
    }

    override def isEmpty: Boolean = vertices.isEmpty
  }

}

