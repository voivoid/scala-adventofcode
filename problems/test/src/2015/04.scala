package adventOfCode.problems.tests

object year2015_04 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem04 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("abcdef6090") ==> 43
    }
  }
}
