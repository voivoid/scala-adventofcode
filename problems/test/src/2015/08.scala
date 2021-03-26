package adventOfCode.problems.tests

object year2015_08 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem08 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("\"\"") ==> 2
      problem.solve1("\"abc\"") ==> 2
      problem.solve1("\"aaa\\\"aaa\"") ==> 3
      problem.solve1("\"\\x27\"") ==> 5
    }

    test("solve2 base cases") {
      problem.solve2("\"\"") ==> 4
      problem.solve2("\"abc\"") ==> 4
      problem.solve2("\"aaa\\\"aaa\"") ==> 6
      problem.solve2("\"\\x27\"") ==> 5
    }
  }
}
