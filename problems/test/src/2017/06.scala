package adventOfCode.problems.tests

object year2017_06 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem06 => problem}

  def input = "0\t2\t7\t0"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 5
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 4
    }
  }
}
