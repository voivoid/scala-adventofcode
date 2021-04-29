package adventOfCode.problems.tests

object year2015_20 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem20 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("10000") ==> 360
    }

    test("solve2 base cases") {
      problem.solve2("10000") ==> 336
    }
  }
}
