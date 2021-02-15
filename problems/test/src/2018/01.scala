package adventOfCode.problems.tests

object year2018_01 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem01 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("+1\n+1\n+1") ==> 3
      problem.solve1("+1\n+1\n-2") ==> 0
      problem.solve1("-1\n-2\n-3") ==> -6
    }

    test("solve2 base cases") {
      problem.solve2("+1\n-1") ==> 0
      problem.solve2("+3\n+3\n+4\n-2\n-4") ==> 10
      problem.solve2("-6\n+3\n+8\n+5\n-6") ==> 5
      problem.solve2("+7\n+7\n-2\n-7\n-4") ==> 14
    }
  }
}
