package adventOfCode.problems.tests

object year2019_01 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem01 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("12") ==> 2
      problem.solve1("14") ==> 2
      problem.solve1("1969") ==> 654
      problem.solve1("100756") ==> 33583
    }

    test("solve2 base cases") {
      problem.solve2("14") ==> 2
      problem.solve2("1969") ==> 966
      problem.solve2("100756") ==> 50346
    }
  }
}
