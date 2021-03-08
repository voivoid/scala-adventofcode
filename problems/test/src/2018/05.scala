package adventOfCode.problems.tests

object year2018_05 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem05 => problem}

  def input = "dabAcCaCBAcCcaDA"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 10
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 4
    }

  }
}
