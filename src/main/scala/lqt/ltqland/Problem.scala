package lqt.ltqland

trait Problem {
  type City = String
  def distanceThroughCities(cities: City*): String

  def totalPathsWithBoundedStops(from: City, to: City, maxStops: Int): Int

  def pathsWithExactStops(from: City, to: City, exactStops: Int): Int

  def totalPathWithBoundedWeight(from: City, to: City, maxWeight: Int): Int

  def lengthOfShortestPath(from: City, to: City): String
}