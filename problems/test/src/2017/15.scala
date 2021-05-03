package adventOfCode.problems.tests

object year2017_15 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem15 => problem}

  def input = "65\n8921"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input, 5) ==> 1
    }

    test("solve2 base cases") {
      problem.solve2(input, 1057) ==> 1
    }
  }
}
