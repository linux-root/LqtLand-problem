package ltq.digraph.dijkstra

import ltq.digraph.Digraph.Weight
import ltq.digraph.dijkstra.DijkstraAlgorithmWithSortedSet.{Distance, Storage}
import ltq.digraph.{Digraph, ShortestPathTree}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.Try


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
    implicit val ordering: Ordering[Distance[T]] = Ordering.fromLessThan{(a, b) =>
      if (a.weight == b.weight) {
        a.vertex != b.vertex
      } else {
        a.weight < b.weight
      }
    }
    Storage(SortedSet(Distance(root, 0)), Set.empty)
  }
}

object DijkstraAlgorithmWithSortedSet {
  case class Distance[T](vertex: T, weight: Weight)
  private case class Storage[T](distances: SortedSet[Distance[T]],
                                visitedVertices: Set[T]) extends ProcessStorage[T] {

    private lazy val headAndTail: (T, SortedSet[Distance[T]]) = {
      @tailrec
      def loop(dists: SortedSet[Distance[T]]): (T,  SortedSet[Distance[T]]) = {
        val headVertex = dists.head.vertex
        if (visitedVertices.contains(headVertex)){
          loop(dists.tail)
        } else {
          (headVertex, dists.tail)
        }
      }
      loop(this.distances)
    }

    override def head: T = this.headAndTail._1
    override def tail: ProcessStorage[T] = {
      val (head, tail) = this.headAndTail
      Storage(tail, visitedVertices + head)
    }

    override def add(xs: Iterable[(T, Weight)]): ProcessStorage[T] = {
      val dists = xs.map{case (v, weight) => Distance(v, weight)}
      Storage(this.distances ++ dists, this.visitedVertices)
    }
    override def isEmpty: Boolean = Try(this.head).isFailure
  }

}
