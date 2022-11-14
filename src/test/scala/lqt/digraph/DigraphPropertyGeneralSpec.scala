package lqt.digraph

import ltq.digraph.Digraph
import ltq.digraph.Digraph.{Edge, Weight}
import org.scalacheck.Gen

import scala.annotation.tailrec

trait DigraphPropertyGeneralSpec {
  val randomWeight: Gen[Weight] = Gen.chooseNum(1, 10000)

  def genConnectedDigraph[T: Gen]: Gen[Digraph[T]] = {
    for {
      totalVertices <- Gen.chooseNum(10, 500)
      graph <- genConnectedDigraph[T](totalVertices, logProgress = false)
    } yield {
      println(s"Created graph with ${graph.vertices.size} vertices")
      graph
    }
  }

  def genConnectedDigraph[T: Gen](totalVertices: Int, logProgress: Boolean = true): Gen[Digraph[T]] = {

    def log(n: Int): Unit = {
      val luckyNumber = 101
      if (n % luckyNumber == 0) {
        println(s"${Console.RESET} Generated graph with ${Console.YELLOW} $n ${Console.RESET} vertices ${Console.GREEN} (${n*100/totalVertices} %)...")
      }
    }

    @tailrec
    def extendGraph(graph: Digraph[T]): Gen[Digraph[T]] = {
      if (logProgress) log(graph.vertices.size)
      if (graph.vertices.size == totalVertices) {
        graph
      } else {
        genEdge(graph).sample match {
          case Some(nextEdge) =>
            extendGraph(graph.addEdge(nextEdge))
          case None =>
            graph
        }
      }
    }

    for {
      seed <- randomSingleVertexGraph
      graph <- extendGraph(seed)
    } yield graph

  }

  def randomSingleVertexGraph[T: Gen]: Gen[Digraph[T]] = {
    for {
      start <- implicitly[Gen[T]]
      end <- implicitly[Gen[T]]
      weight <- randomWeight
    } yield Digraph(Set(Edge(start, end, weight)))
  }

  private def genEdge[T: Gen](graph: Digraph[T]): Gen[Edge[T]] = {
    val vertices = graph.vertices
    for {
      weight <- randomWeight
      vertex1 <- Gen.oneOf(vertices)
      newValue <- implicitly[Gen[T]].filterNot(vertices.contains)
      edgeWithNewValue <- Gen.oneOf(Edge(newValue, vertex1, weight), Edge(vertex1, newValue, weight))
      edge <- vertices.filterNot(_ == vertex1).find(v => !graph.directlyConnected(vertex1, v) || !graph.directlyConnected(v, vertex1)) match {
        case None =>
          Gen.const(edgeWithNewValue)
        case Some(vertex2) =>
          val edge21 = Edge(vertex2, vertex1, weight)
          val edge12 = Edge(vertex1, vertex2, weight)
          if (graph.directlyConnected(vertex1, vertex2)) {
            Gen.oneOf(edge21, edge21, edgeWithNewValue)
          } else if (graph.directlyConnected(vertex2, vertex1)) {
            Gen.oneOf(edge12, edge12, edgeWithNewValue)
          } else {
            Gen.frequency(
              2 -> Gen.oneOf(edge12, edge21),  // increase probability creating cycle in graph
              1 -> Gen.const(edgeWithNewValue)
            )
          }
      }
    } yield edge
  }

}
