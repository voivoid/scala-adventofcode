package adventOfCode.problems.tests

object year2020_12 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem12 => problem}

  def input = """F10
                |N3
                |F7
                |R90
                |F11""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 25
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 286
    }

  }
}
