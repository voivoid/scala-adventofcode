package adventOfCode.problems.tests

object year2019_10 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem10 => problem}

  def input1 = """.#..#
                 |.....
                 |#####
                 |....#
                 |...##""".stripMargin

  def input2 = """......#.#.
                 |#..#.#....
                 |..#######.
                 |.#.#.###..
                 |.#..#.....
                 |..#....#.#
                 |#..#....#.
                 |.##.#..###
                 |##...#..#.
                 |.#....####""".stripMargin

  def input3 = """#.#...#.#.
                 |.###....#.
                 |.#....#...
                 |##.#.#.#.#
                 |....#.#.#.
                 |.##..###.#
                 |..#...##..
                 |..##....##
                 |......#...
                 |.####.###.""".stripMargin

  def input4 = """.#..#..###
                  |####.###.#
                  |....###.#.
                  |..###.##.#
                  |##.##.#.#.
                  |....###..#
                  |..#.#..#.#
                  |#..#.#.###
                  |.##...##.#
                  |.....#.#..""".stripMargin

  def input5 = """.#..##.###...#######
                 |##.############..##.
                 |.#.######.########.#
                 |.###.#######.####.#.
                 |#####.##.#.##.###.##
                 |..#####..#.#########
                 |####################
                 |#.####....###.#.#.##
                 |##.#################
                 |#####.##.###..####..
                 |..######..##.#######
                 |####.##.####...##..#
                 |.#####..#.######.###
                 |##...#.##########...
                 |#.##########.#######
                 |.####.#.###.###.#.##
                 |....##.##.###..#####
                 |.#.#.###########.###
                 |#.#.#.#####.####.###
                 |###.##.####.##.#..##""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 8
      problem.solve1(input2) ==> 33
      problem.solve1(input3) ==> 35
      problem.solve1(input4) ==> 41
      problem.solve1(input5) ==> 210
    }

    test("solve2 base cases") {
      problem.solve2(input1, 1) ==> 302

      problem.solve2(input5, 1) ==> 1112
      problem.solve2(input5, 2) ==> 1201
      problem.solve2(input5, 3) ==> 1202

      problem.solve2(input5) ==> 802

      problem.solve2(input5, 299) ==> 1101
    }
  }
}
