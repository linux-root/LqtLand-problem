package lqt.ltqland

import com.typesafe.config.Config
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait Configuration {
  protected def config: Config

  /**
   * Model the route in City
   */
  case class Route(from: String, to: String, distance: Int)

  protected val routes: List[Route] = config.getStringList("map.routes").asScala.map { routeStr =>
    val regex = "^\\w{2}\\d+$".r
    if (regex.matches(routeStr)) {
      val from = routeStr(0).toString
      val to = routeStr(1).toString
      val distance = routeStr.filter(_.isDigit).toInt
      Route(from, to, distance)
    } else {
      throw new IllegalArgumentException(s"Config is invalid. Cannot parse route value $routeStr")
    }
  }.toList

}
