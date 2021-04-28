package adventOfCode.problems.tests

object year2015_18 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem18 => problem}

  def input1 = """.#.#.#
                |...##.
                |#....#
                |..#...
                |#.#..#
                |####..""".stripMargin

  def input2 = """##.#.#
                 |...##.
                 |#....#
                 |..#...
                 |#.#..#
                 |####.#""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1, steps = 4) ==> 4
    }

    test("solve2 base cases") {
      problem.solve2(input2, steps = 5) ==> 17
    }
  }
}
