package adventOfCode.problems.tests

object year2016_02 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem02 => problem}

  val input = "ULL\nRRDDD\nLURDL\nUUUUD"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> "1985"
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> "5DB3"
    }
  }
}
