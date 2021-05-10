package adventOfCode.problems.tests

object year2019_17 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem17 => problem}

  def input1 =
    """..#..........
    |..#..........
    |#######...###
    |#.#...#...#.#
    |#############
    |..#...#...#..
    |..#####...<..""".stripMargin

  def input2 =
    """#######...#####
    |#.....#...#...#
    |#.....#...#...#
    |......#...#...#
    |......#...###.#
    |......#.....#.#
    |^########...#.#
    |......#.#...#.#
    |......#########
    |........#...#..
    |....#########..
    |....#...#......
    |....#...#......
    |....#...#......
    |....#####......""".stripMargin

  val tests = Tests {
    test("impl tests") {
      import problem._
      import adventOfCode.utils.path.Turn.{Right, Left}

      val grid1 = input1.split('\n')
      val grid2 = input2.split('\n')

      countIntersections(grid1) ==> 76

      assertMatch(findPath(grid1)) {
        case List(
              Cmd(Right, 4),
              Cmd(Right, 2),
              Cmd(Right, 2),
              Cmd(Right, 12),
              Cmd(Right, 2),
              Cmd(Right, 6),
              Cmd(Right, 4),
              Cmd(Right, 4),
              Cmd(Right, 6)
            ) =>
      }

      val Path2 = List(
        Cmd(Right, 8),
        Cmd(Right, 8),
        Cmd(Right, 4),
        Cmd(Right, 4),
        Cmd(Right, 8),
        Cmd(Left, 6),
        Cmd(Left, 2),
        Cmd(Right, 4),
        Cmd(Right, 4),
        Cmd(Right, 8),
        Cmd(Right, 8),
        Cmd(Right, 8),
        Cmd(Left, 6),
        Cmd(Left, 2)
      )

      assertMatch(findPath(grid2)) { case Path2 =>
      }
    }
  }
}
