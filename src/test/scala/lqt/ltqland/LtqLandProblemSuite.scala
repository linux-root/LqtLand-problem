package lqt.ltqland

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

trait LtqLandProblemSuite extends Problem with AnyFunSuiteLike with Matchers {

  test("1. The distance of the route A-B-C") {
    this.distanceThroughCities("A", "B", "C") shouldBe "9"
  }

  test("2. The distance of the route A-D") {
    this.distanceThroughCities("A", "D") shouldBe "5"
  }

  test("3. The distance of the route A-D-C") {
    this.distanceThroughCities("A", "D", "C") shouldBe "13"
  }

  test("4. The distance of the route A-E-B-C-D") {
    this.distanceThroughCities("A", "E", "B", "C", "D") shouldBe "22"
  }

  test("5. The distance of the route A-E-D") {
    this.distanceThroughCities("A", "E", "D") shouldBe "NO SUCH ROUTE"
  }

  test("""6. The number of trips starting at C and ending at C with a maximum of 3 stops.
      In the sample data below, there are two such trips: C-D-C (2 stops),
      and C-E-B-C (3 stops).""") {

    /** FIXME ?
     * Based on the example, the term "stop" refers to the unique city along the trips,
     * However, with that meaning, the number of paths go through C, D will be infinite, e.g :
     * path C-D-C-C-D-C also has 2 "stop", but we can create infinite amount of paths just by adding another
     * loop C-D-C.
     * Moreover, an example in 7. shows that path A to C (via D,C,D) has 4 stops, which is conflicted
     * with above definition of "stop"
     *
     * The following solution assumes that "stop" is the number of cities along
     * the path, including visited city. Therefore, C-D-C will have 3 stops; C-E-B-C will have 4 stops
     * To obtain the same 2 paths above, we find the path with a maximum of 4 stops.
     */
    this.totalPathsWithBoundedStops("C", "C", 4) shouldBe 2
  }

  test("""7. The number of trips starting at A and ending at C with exactly 4 stops.
     In the sample data below, there are three such trips: A to C (via B,C,D);
     A to C (via D,C,D); and A to C (via D,E,B). """) {
    /**
     * This solution also uses the term of "stop" described in 6.
     */
    this.pathsWithExactStops("A", "C", 5) shouldBe 3
  }

  test("8. The length of the shortest route (in terms of distance to travel) from A to C.") {
    this.lengthOfShortestPath("A", "C") shouldBe "9"
  }

  test("9. The length of the shortest route (in terms of distance to travel) from B to B.") {
    this.lengthOfShortestPath("B", "B") shouldBe "0"
  }

  test("""10. The number of different routes from C to C with a distance of less than 30.
    In the sample data, the trips are: CDC, CEBC, CEBCDC, CDCEBC, CDEBC, CEBCEBC, CEBCEBCEBC.""") {
    this.totalPathWithBoundedWeight("C", "C", 30) shouldBe 7
  }
}