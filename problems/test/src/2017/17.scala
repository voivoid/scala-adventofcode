package adventOfCode.problems.tests

object year2017_17 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem17 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("3") ==> 638
    }
  }
}
