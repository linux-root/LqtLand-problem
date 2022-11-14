package lqt.digraph

import ltq.digraph.{Digraph, ShortestPathTree}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

trait ShortestPathTreeBenchmark[T] extends AnyWordSpec with ScalaCheckPropertyChecks with DigraphPropertyGeneralSpec with Matchers {

  protected def algorithmName: String

  protected def genValue: Gen[T]

  private implicit val _genValue: Gen[T] = genValue

  protected def createTree(root: T, graph: Digraph[T]): ShortestPathTree[T]

  protected def totalVertices: Int = 2500

  protected def expectedMaxExecutionTimeInMillis: Long

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  def timer[A](f: => A): (A, Long) = {
    val start = System.currentTimeMillis()
    val result = f
    (result, System.currentTimeMillis() - start)
  }

  s"""Benchmark time to build Shortest-path tree of ${this.totalVertices} vertices
      Using algorithm : $algorithmName""" in {
    forAll(genConnectedDigraph[T](totalVertices)) { graph =>
      val buildAndEvaluateTree: T => Unit = vertex => this.createTree(vertex, graph).predecessorOf(vertex)
      val (_, executionTimeInMillis) = timer(graph.vertices.map(buildAndEvaluateTree))
      println(s"""__ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
            | It takes $executionTimeInMillis milliseconds, to build SPTs for ${this.totalVertices} vertices by $algorithmName |
           """)
      graph.vertices.size shouldBe totalVertices
      executionTimeInMillis should be <= this.expectedMaxExecutionTimeInMillis
    }

  }
}
