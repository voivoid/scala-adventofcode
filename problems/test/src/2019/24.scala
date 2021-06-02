package adventOfCode.problems.tests

object year2019_24 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem24 => problem}

  def input = """....#
                |#..#.
                |#..##
                |..#..
                |#....""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 2129920
    }

    test("solve2 base cases") {
      problem.solve2(input, 10) ==> 99
    }
  }
}
