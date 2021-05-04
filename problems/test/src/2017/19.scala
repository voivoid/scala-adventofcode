package adventOfCode.problems.tests

object year2017_19 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem19 => problem}

  def input =
    "     |          \n" +
      "     |  +--+    \n" +
      "     A  |  C    \n" +
      " F---|----E|--+ \n" +
      "     |  |  |  D \n" +
      "     +B-+  +--+ "

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> "ABCDEF"
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 38
    }
  }
}
