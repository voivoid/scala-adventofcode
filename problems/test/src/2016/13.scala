package adventOfCode.problems.tests

object year2016_13 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem13 => problem}
  import adventOfCode.utils.geo.Point

  val tests = Tests {
    test("solve1 base cases") {
      val finalPos = Point(7, 4)
      problem.solve1("10", finalPos) ==> 11
    }

    test("impl tests") {
      import problem._

      val office = 10
      assert(!isWall(Point(0, 0), office))
      assert(isWall(Point(0, 6), office))

      assert(!isWall(Point(3, 3), office))
      assert(!isWall(Point(4, 4), office))
      assert(!isWall(Point(5, 5), office))

      assert(isWall(Point(9, 0), office))
      assert(isWall(Point(9, 6), office))
    }
  }
}
