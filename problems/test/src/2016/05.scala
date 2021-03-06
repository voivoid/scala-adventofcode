package adventOfCode.problems.tests

object year2016_05 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem05 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("abc") ==> "18f47a30"
    }
  }
}
