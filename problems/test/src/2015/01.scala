package adventOfCode.problems.tests

object year2015_01 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem01 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("()()") ==> 0
      problem.solve1("(())") ==> 0
      problem.solve1("))(((((") ==> 3
      problem.solve1("())") ==> -1
      problem.solve1("))(") ==> -1
      problem.solve1(")))") ==> -3
      problem.solve1(")())())") ==> -3
    }

    test("solve2 base cases") {
      problem.solve2(")") ==> 1
      problem.solve2("()())") ==> 5
    }
  }
}
