package adventOfCode.problems.tests

object year2016_03 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem03 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("5 10 12") ==> 1
      problem.solve1("5 10 25") ==> 0
    }

    test("solve2 base cases") {
      problem.solve2("101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603") ==> 6
    }
  }
}
