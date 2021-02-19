package adventOfCode.problems.tests

object year2015_02 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem02 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("2x3x4") ==> 58
      problem.solve1("1x1x10") ==> 43
    }

    test("solve2 base cases") {
      problem.solve2("2x3x4") ==> 34
      problem.solve2("1x1x10") ==> 14
    }
  }
}
