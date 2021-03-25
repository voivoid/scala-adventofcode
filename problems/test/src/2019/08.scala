package adventOfCode.problems.tests

object year2019_08 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem08 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("110022011222", 3, 2) ==> 6
    }

    test("solve2 base cases") {
      problem.decodePixels("0222112222120000", 2, 2) ==> Seq('0', '1', '1', '0')
    }
  }
}
