package lqt.ltqland

import com.typesafe.config.{Config, ConfigFactory}
import ltq.digraph.Digraph
import ltq.digraph.Digraph.{Edge, Path}

trait Solution extends Problem with Configuration {

  override protected def config: Config = ConfigFactory.load()

  private val graph: Digraph[City] = Digraph[City](routes.map(r => Edge(r.from, r.to, r.distance)).toSet)

  def distanceThroughCities(cities: City*): String = {
    graph.totalWeight(cities.toList) match {
      case None => "NO SUCH ROUTE"
      case Some(distance) => distance.toString
    }
  }

  def totalPathsWithBoundedStops(from: City, to: City, maxStops: Int): Int = {
    val stopWhen: Path[City] => Boolean = path => path.size >= maxStops
    val paths = graph.findAllPath(from, to, stopWhen)()
    paths.size
  }

  def pathsWithExactStops(from: City, to: City, exactStops: Int): Int = {
    val stopWhen: Path[City] => Boolean = path => path.size > exactStops
    val paths = graph.findAllPath(from, to, stopWhen)(path => path.vertices.size == exactStops)
    paths.size
  }


  def totalPathWithBoundedWeight(from: City, to: City, maxWeight: Int): Int = {
    val stopWhen: Path[City] => Boolean = path => path.weight >= maxWeight
    val paths = graph.findAllPath(from, to, stopWhen)(path => path.weight < maxWeight)
    paths.size
  }

  def lengthOfShortestPath(from: City, to: City): String = {
    graph.shortestDistance(from, to) match {
      case None => "NO SUCH ROUTE"
      case Some(d) => d.toString
    }
  }

}
