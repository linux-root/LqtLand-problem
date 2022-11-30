package ltq.digraph.dijkstra

import ltq.digraph.Digraph.Weight

import scala.annotation.tailrec


trait ProcessStorage[T] {
  def head: T
  def tail: ProcessStorage[T]
  def add(xs: Iterable[(T, Weight)]): ProcessStorage[T]
  def isEmpty: Boolean
}


trait DijkstraAlgorithm[T] {
  type Predecessors = Map[T, T]
  type ShortestDistances = Map[T, Weight]
  private val MAX_WEIGHT: Weight = Int.MaxValue


  protected lazy val (distances, predecessors) : (ShortestDistances, Predecessors) = {
    calculateTree(initStorage(this.root), Map(this.root -> 0), Map.empty)
  }

  protected def initStorage(root: T): ProcessStorage[T]

  protected def graph: T => Map[T, Weight]

  def root: T

  @tailrec
  final protected def calculateTree(toBeExplored: ProcessStorage[T],
                          distances: ShortestDistances, tree: Predecessors): (ShortestDistances, Predecessors) = {
    if (toBeExplored.isEmpty) {
      (distances, tree)
    } else {
      val currentVertex = toBeExplored.head
      val weight = distances(currentVertex)
      val neighbors = for {
        (v, w) <- graph(currentVertex).filter{case (vertex, _) => vertex != this.root}
        if weight + w < distances.getOrElse(v, MAX_WEIGHT)
      } yield v -> (weight + w)

      val updatedTree = tree ++ neighbors.view.mapValues(_ => currentVertex)
      val updatedDistances = distances ++ neighbors
      val updatedRemaining = toBeExplored.tail.add(neighbors)
      calculateTree(updatedRemaining, updatedDistances, updatedTree)
    }
  }

}

