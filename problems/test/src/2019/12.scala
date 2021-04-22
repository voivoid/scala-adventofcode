package adventOfCode.problems.tests

object year2019_12 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem12 => problem}

  def input1 =
    """<x=-1, y=0, z=2>
      |<x=2, y=-10, z=-7>
      |<x=4, y=-8, z=8>
      |<x=3, y=5, z=-1>""".stripMargin

  def input2 =
    """<x=-8, y=-10, z=0>
      |<x=5, y=5, z=10>
      |<x=2, y=-7, z=3>
      |<x=9, y=-8, z=-3>""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1, 10) ==> 179
      problem.solve1(input2, 100) ==> 1940
    }

    test("solve2 base cases") {
      problem.solve2(input1) ==> 2772L
      problem.solve2(input2) ==> 4686774924L
    }

    test("impl tests") {
      import problem._
      import adventOfCode.utils.geo.Point3D

      assertMatch(problem.parseMoon("<x=-1, y=0, z=2>")) { case Moon(Point3D(-1, 0, 2), Point3D(0, 0, 0)) =>
      }
    }
  }
}
