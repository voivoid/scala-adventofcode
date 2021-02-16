package adventOfCode.problems.tests

object year2020_01 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem01 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("1721\n979\n366\n299\n675\n1456") ==> 514579
    }

    test("solve2 base cases") {
      problem.solve2("1721\n979\n366\n299\n675\n1456") ==> 241861950
    }
  }
}
