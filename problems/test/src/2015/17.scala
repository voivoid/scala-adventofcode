package adventOfCode.problems.tests

object year2015_17 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem17 => problem}

  def input = "20\n15\n10\n5\n5"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input, 25) ==> 4
    }

    test("solve2 base cases") {
      problem.solve2(input, 25) ==> 3
    }
  }
}
