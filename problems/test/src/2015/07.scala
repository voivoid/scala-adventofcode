package adventOfCode.problems.tests

object year2015_07 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem07 => problem}

  val tests = Tests {
    test("impl tests") {
      problem.implTests()
    }
  }
}
