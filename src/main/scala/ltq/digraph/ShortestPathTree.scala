package ltq.digraph

import ltq.digraph.Digraph.Weight


trait ShortestPathTree[T] {
  def root: T

  protected def graph: Digraph[T]

  def distanceTo(vertex: T): Option[Weight]

  def predecessorOf(vertex: T): Option[T]
}

object ShortestPathTree {
  def apply[T](v: T, g: Digraph[T]): ShortestPathTree[T] = new DijkstraAlgorithm[T] {
    override val root: T = v

    override protected val graph: Digraph[T] = g
  }
}