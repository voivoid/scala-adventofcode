package adventOfCode.problems.tests

object year2015_03 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem03 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(">") ==> 2
      problem.solve1("^>v<") ==> 4
      problem.solve1("^v^v^v^v^v") ==> 2
    }

    test("solve2 base cases") {
      problem.solve2("^v") ==> 3
      problem.solve2("^>v<") ==> 3
      problem.solve2("^v^v^v^v^v") ==> 11
    }
  }
}
