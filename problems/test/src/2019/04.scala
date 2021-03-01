package adventOfCode.problems.tests

object year2019_04 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem04 => problem}

  val tests = Tests {
    test("impl tests") {
      problem.implTests()
    }
  }
}
