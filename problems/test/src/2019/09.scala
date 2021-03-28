package adventOfCode.problems.tests

object year2019_09 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem09 => problem}

  val tests = Tests {
    test("impl tests") {
      problem.implTests()
    }
  }
}
