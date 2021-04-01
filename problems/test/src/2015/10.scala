package adventOfCode.problems.tests

object year2015_10 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem10 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve("1", 1) ==> 2
      problem.solve("11", 1) ==> 2
      problem.solve("21", 1) ==> 4
      problem.solve("1211", 1) ==> 6
      problem.solve("111221", 1) ==> 6

      problem.solve("1", 4) ==> 6
    }
  }
}
