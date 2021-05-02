package adventOfCode.problems.tests

object year2015_25 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem25 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("row 1, column 1.") ==> 20151125
      problem.solve1("row 6, column 1.") ==> 33071741
      problem.solve1("row 1, column 6.") ==> 33511524
      problem.solve1("row 6, column 6.") ==> 27995004
    }

  }
}
