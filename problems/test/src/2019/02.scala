package adventOfCode.problems.tests

object year2019_02 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem02 => problem}

  val tests = Tests {
    test("impl tests") {
      problem.implTests()
    }
  }
}
