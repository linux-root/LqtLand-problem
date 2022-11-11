package ltq.digraph

import ltq.digraph.Digraph.{Edge, Path}

import scala.annotation.tailrec
import scala.util.Try


/**
 *
 * Edge-weighted Directed Graph
 *
 * @author Watson Dinh
 *
 */
trait Digraph[T] {

  /**
   * @return Edges pointing from @param vertex
   */
  def adjacent(vertex: T): Set[Edge[T]]

  /**
   *
   * @return None in case vertices are not connected or
   *         there's no way to go from one vertex to the next one
   */
  def totalWeight(vertices: List[T]): Option[Int]

  def edges: Set[Edge[T]]

  def addEdge(edge: Edge[T]): Digraph[T]

  def vertices: Set[T]

  def directlyConnected(from: T, to: T): Boolean = {
    this.adjacent(from).exists(_.end == to)
  }

  /**
   * @stop
   * @return all paths from @ source to @param target
   *         with size < @param maxSize which meet the @param condition
   */
  def findAllPath(source: T, target: T, stopCondition: Path[T] => Boolean)
                 (condition: Path[T] => Boolean = _ => true): List[Path[T]]

  def shortestDistance(source: T, target: T): Option[Int]

  override def toString: String = {
    this.edges.mkString(" ")
  }
}


object Digraph {

  type Weight = Int

  /**
   *
   * @param vertices all vertices along the path, in ordered
   * @param weight   total weight of all edges along the path
   */
  case class Path[T](vertices: List[T], weight: Weight) {
    lazy val size: Int = vertices.size

    def extend(vertex: T, weight: Weight): Path[T] = {
      this.copy(vertices = this.vertices :+ vertex, weight = this.weight + weight)
    }
  }

  object Path {
    def start[T](startVertex: T): Path[T] = Path(List(startVertex), 0)
  }

  case class Edge[T](start: T, end: T, weight: Weight) {
    override def toString: String = {
      s"($start)--$weight-->($end)"
    }

    override def hashCode(): Int = {
      41 * (41 + start.hashCode()) + end.hashCode()
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case Edge(start, end, _) =>
           start == this.start && end == this.end
        case _ =>
          false
      }
    }
  }

  object Edge {

    def parseStrEdge(s: String): Option[Edge[String]] = {
      val weightPattern = "--\\d+-->".r
      for {
        (start, end) <- Try {
          val matchedStr = s.split(weightPattern.regex).map(_.tail.dropRight(1))
          (matchedStr(0), matchedStr(1))
        }.toOption
        weight <- weightPattern.findFirstIn(s).map(_.drop(2).dropRight(3).toInt)
      } yield Edge(start, end, weight)
    }

    def parseStrEdges(s: String): List[Edge[String]] = {
      s.split("\\s+").toList.map(parseStrEdge).filter(_.nonEmpty).map(_.get)
    }
  }

  def apply[T](edgesList: Set[Edge[T]]): Digraph[T] = new Digraph[T] {

    private val _adjacentMap = edgesList.groupBy(_.start).view.mapValues(_.toSet)

    private var _shortestPathTree: Map[T, ShortestPathTree[T]] = Map.empty

    override def adjacent(vertex: T): Set[Edge[T]] = {
      this._adjacentMap.getOrElse(vertex, Set.empty)
    }

    override val edges: Set[Edge[T]] = edgesList

    override val vertices: Set[T] = this.edges.flatMap(edge => List(edge.start, edge.end))

    override def findAllPath(source: T, target: T, stopCondition: Path[T] => Boolean)
                            (condition: Path[T] => Boolean = _ => true): List[Path[T]] = {
      @tailrec
      def find(verticesWithPath: List[(T, Path[T])], foundPaths: List[Path[T]]): List[Path[T]] = {
        verticesWithPath match {
          case Nil =>
            foundPaths
          case (vertex, path) :: tail =>
            val neighbors = if (stopCondition(path)) Nil // stop searching further on neighbors when path to current vertex satisfied stopCondition
            else adjacent(vertex).map(edge => (edge.end, path.extend(edge.end, edge.weight)))
            val found = vertex == target && condition(path) && path.size > 1
            val updatedFoundPaths = if (found) path :: foundPaths else foundPaths
            find(tail ++ neighbors, updatedFoundPaths)
        }
      }

      find(List(source -> Path.start(source)), foundPaths = Nil)
    }

    override def totalWeight(vertices: List[T]): Option[Int] = {
      @tailrec
      def calculateWeight(vertices: List[T], previousVertex: T, weightAcc: Int): Option[Int] = {
        vertices match {
          case Nil => Some(weightAcc)
          case currentVertex :: tail =>
            adjacent(previousVertex).find(_.end == currentVertex) match {
              case None => None
              case Some(Edge(_, _, weight)) =>
                calculateWeight(tail, currentVertex, weightAcc + weight)
            }
        }
      }

      calculateWeight(vertices.tail, vertices.head, 0)
    }

    override def shortestDistance(from: T, target: T): Option[Weight] = {
      val shortestPathTree = this._shortestPathTree.getOrElse(from,
        {
          val newTree = ShortestPathTree(from, this)
          this._shortestPathTree = this._shortestPathTree + (from -> newTree)
          newTree
        })
      shortestPathTree distanceTo target
    }

    override def addEdge(edge: Edge[T]): Digraph[T] = {
      Digraph(this.edges + edge)
    }
  }
}
