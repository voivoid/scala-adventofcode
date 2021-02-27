package adventOfCode.problems.tests

object year2016_01 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem01 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("R2, L3") ==> 5
      problem.solve1("R2, R2, R2") ==> 2
      problem.solve1("R5, L5, R5, R3") ==> 12
    }

    test("solve2 base cases") {
      problem.solve2("R8, R4, R4, R8") ==> 4
    }
  }
}
