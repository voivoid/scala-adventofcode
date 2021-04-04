package adventOfCode.problems.tests

object year2018_03 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem03 => problem}

  val input = """#1 @ 1,3: 4x4
                |#2 @ 3,1: 4x4
                |#3 @ 5,5: 2x2""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 4
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 3
    }

    test("impl tests") {
      import problem._
      import adventOfCode.utils.geo.{Point, Rect}

      assertMatch(parseClaim("#1 @ 1,3: 4x4")) { case Claim(1, Rect(Point(1, 3), Point(4, 6))) => }

      assertMatch(divideRect(Rect(Point(0, 0), Point(3, 3)))) {
        case List(
              Rect(Point(0, 0), Point(1, 1)),
              Rect(Point(0, 2), Point(1, 3)),
              Rect(Point(2, 0), Point(3, 1)),
              Rect(Point(2, 2), Point(3, 3))
            ) =>
      }

      assertMatch(divideRect(Rect(Point(0, 0), Point(4, 4)))) {
        case List(
              Rect(Point(0, 0), Point(2, 2)),
              Rect(Point(0, 3), Point(2, 4)),
              Rect(Point(3, 0), Point(4, 2)),
              Rect(Point(3, 3), Point(4, 4))
            ) =>
      }
    }
  }
}
