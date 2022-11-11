package lqt.digraph

import ltq.digraph.Digraph
import ltq.digraph.Digraph.{Edge, Weight}
import org.scalacheck.Gen

trait DigraphPropertyGeneralSpec {
  val randomWeight: Gen[Weight] = Gen.chooseNum(1, 500)

  def genConnectedDigraph[T: Gen]: Gen[Digraph[T]] = {
    def extendGraph(graph: Digraph[T]): Gen[Digraph[T]] = {
      println(s"extendGraph $graph")
      for {
        nextEdge <- genEdge(graph)
        graph <- Gen.oneOf(Gen.const(graph), extendGraph(graph.addEdge(nextEdge)))
      } yield graph
    }

    for {
      seed <- randomSingleVertexGraph
      graph <- extendGraph(seed)
    } yield graph
  }

  def randomSingleVertexGraph[T: Gen]: Gen[Digraph[T]] = {
    println("GenSingleVertex")
    for {
      start <- implicitly[Gen[T]]
      end <- implicitly[Gen[T]]
      weight <- randomWeight
    } yield Digraph(Set(Edge(start, end, weight)))
  }

  private def genEdge[T: Gen](graph: Digraph[T]): Gen[Edge[T]] = {
    val vertices = graph.vertices
    for {
      newValue <- implicitly[Gen[T]].filterNot(vertices.contains)
      vertex1 <- Gen.oneOf(vertices)
      vertex2 <- vertices.filterNot(_ == vertex1).find(v => !graph.directlyConnected(vertex1, v) || !graph.directlyConnected(v, vertex1)) match {
        case None =>
          Gen.const(newValue)
        case Some(v) =>
          Gen.oneOf(v, newValue)
      }
      weight <- randomWeight
      edge <- Gen.oneOf(Edge(vertex2, vertex1, weight), Edge(vertex1, vertex2, weight))
    } yield edge
  }

}
