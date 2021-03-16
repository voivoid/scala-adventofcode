package adventOfCode.problems.tests

object year2015_06 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem06 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("turn on 499,499 through 500,500") ==> 4
      problem.solve1("toggle 0,0 through 999,0") ==> 1000
    }

    test("solve2 base cases") {
      problem.solve2("turn on 0,0 through 0,0") ==> 1
      problem.solve2("toggle 0,0 through 999,0") ==> 2000
    }

    test("impl tests") {
      problem.implTests()
    }
  }
}
