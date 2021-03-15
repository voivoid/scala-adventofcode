package adventOfCode.problems.tests

object year2018_06 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem06 => problem}

  def input = """1, 1
                |1, 6
                |8, 3
                |3, 4
                |5, 5
                |8, 9""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 17
    }

    test("solve2 base cases") {
      problem.solve2(input, maxDistance = 32) ==> 16
    }

  }
}
