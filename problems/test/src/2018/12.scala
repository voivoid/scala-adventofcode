package adventOfCode.problems.tests

object year2018_12 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem12 => problem}

  def input = """initial state: #..#.#..##......###...###
                |
                |...## => #
                |..#.. => #
                |.#... => #
                |.#.#. => #
                |.#.## => #
                |.##.. => #
                |.#### => #
                |#.#.# => #
                |#.### => #
                |##.#. => #
                |##.## => #
                |###.. => #
                |###.# => #
                |####. => #""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 325
    }
  }
}
