package ltq.digraph

import ltq.digraph.Digraph.Weight

import scala.annotation.tailrec


trait DijkstraAlgorithm[T] extends ShortestPathTree[T] {
  type Predecessors = Map[T, T]
  type ShortestDistances = Map[T, Weight]
  private val MAX_WEIGHT: Weight = Int.MaxValue

  private lazy val (distances, predecessors)  : (ShortestDistances, Predecessors) = {
    @tailrec
    def calculateTree(remaining: Set[T], distances: ShortestDistances, tree: Predecessors): (ShortestDistances, Predecessors) = {
      if (remaining.isEmpty) {
        (distances, tree)
      } else {
        val currentVertex = remaining.minBy(distances)
        val weight = distances(currentVertex)
        val neighbors = for {
          (v, w) <- this.graph.adjacent(currentVertex).map(edge => (edge.end, edge.weight)).toMap
          if weight + w < distances.getOrElse(v, MAX_WEIGHT)
        } yield v -> (weight + w)

        val updatedTree = tree ++ neighbors.view.mapValues(_ => currentVertex)
        val updatedDistances = distances ++ neighbors
        val updatedRemaining = (remaining - currentVertex) ++ neighbors.keys

        calculateTree(updatedRemaining, updatedDistances, updatedTree)
      }
    }

    calculateTree(Set(this.root), Map(this.root -> 0), Map.empty)
  }

  override def distanceTo(vertex: T): Option[Weight] = {
    distances.get(vertex)
  }

  override def predecessorOf(vertex: T): Option[T] = {
    this.predecessors.get(vertex)
  }
}

