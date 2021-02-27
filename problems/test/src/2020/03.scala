package adventOfCode.problems.tests

object year2020_03 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem03 => problem}

  val input = """..##.......
                |#...#...#..
                |.#....#..#.
                |..#.#...#.#
                |.#...##..#.
                |..#.##.....
                |.#.#.#....#
                |.#........#
                |#.##...#...
                |#...##....#
                |.#..#...#.#""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 7
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 336
    }
  }
}
