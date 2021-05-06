package adventOfCode.problems.tests

object year2017_22 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem22 => problem}

  def input = "..#\n#..\n..."

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input, 7) ==> 5
      problem.solve1(input, 70) ==> 41
    }

    test("solve2 base cases") {
      problem.solve2(input, 100) ==> 26
    }
  }
}
