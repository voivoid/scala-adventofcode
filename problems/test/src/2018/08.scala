package adventOfCode.problems.tests

object year2018_08 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem08 => problem}

  def input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 138
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 66
    }
  }
}
