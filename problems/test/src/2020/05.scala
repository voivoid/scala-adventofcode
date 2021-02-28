package adventOfCode.problems.tests

object year2020_05 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem05 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("FBFBBFFRLR") ==> 357
      problem.solve1("BFFFBBFRRR") ==> 567
      problem.solve1("FFFBBBFRRR") ==> 119
      problem.solve1("BBFFBBFRLL") ==> 820
    }
  }
}
