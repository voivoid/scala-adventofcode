package adventOfCode.problems.tests

object year2019_20 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem20 => problem}

  // format: off
  def input1 =
      "         A           \n" +
      "         A           \n" +
      "  #######.#########  \n" +
      "  #######.........#  \n" +
      "  #######.#######.#  \n" +
      "  #######.#######.#  \n" +
      "  #######.#######.#  \n" +
      "  #####  B    ###.#  \n" +
      "BC...##  C    ###.#  \n" +
      "  ##.##       ###.#  \n" +
      "  ##...DE  F  ###.#  \n" +
      "  #####    G  ###.#  \n" +
      "  #########.#####.#  \n" +
      "DE..#######...###.#  \n" +
      "  #.#########.###.#  \n" +
      "FG..#########.....#  \n" +
      "  ###########.#####  \n" +
      "             Z       \n" +
      "             Z       "

  def input2 =
      "                   A               \n" +
      "                   A               \n" +
      "  #################.#############  \n" +
      "  #.#...#...................#.#.#  \n" +
      "  #.#.#.###.###.###.#########.#.#  \n" +
      "  #.#.#.......#...#.....#.#.#...#  \n" +
      "  #.#########.###.#####.#.#.###.#  \n" +
      "  #.............#.#.....#.......#  \n" +
      "  ###.###########.###.#####.#.#.#  \n" +
      "  #.....#        A   C    #.#.#.#  \n" +
      "  #######        S   P    #####.#  \n" +
      "  #.#...#                 #......VT\n" +
      "  #.#.#.#                 #.#####  \n" +
      "  #...#.#               YN....#.#  \n" +
      "  #.###.#                 #####.#  \n" +
      "DI....#.#                 #.....#  \n" +
      "  #####.#                 #.###.#  \n" +
      "ZZ......#               QG....#..AS\n" +
      "  ###.###                 #######  \n" +
      "JO..#.#.#                 #.....#  \n" +
      "  #.#.#.#                 ###.#.#  \n" +
      "  #...#..DI             BU....#..LF\n" +
      "  #####.#                 #.#####  \n" +
      "YN......#               VT..#....QG\n" +
      "  #.###.#                 #.###.#  \n" +
      "  #.#...#                 #.....#  \n" +
      "  ###.###    J L     J    #.#.###  \n" +
      "  #.....#    O F     P    #.#...#  \n" +
      "  #.###.#####.#.#####.#####.###.#  \n" +
      "  #...#.#.#...#.....#.....#.#...#  \n" +
      "  #.#####.###.###.#.#.#########.#  \n" +
      "  #...#.#.....#...#.#.#.#.....#.#  \n" +
      "  #.###.#####.###.###.#.#.#######  \n" +
      "  #.#.........#...#.............#  \n" +
      "  #########.###.###.#############  \n" +
      "           B   J   C               \n" +
      "           U   P   P               "

  def input3 =
      "             Z L X W       C                 \n" +
      "             Z P Q B       K                 \n" +
      "  ###########.#.#.#.#######.###############  \n" +
      "  #...#.......#.#.......#.#.......#.#.#...#  \n" +
      "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n" +
      "  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n" +
      "  #.###.#######.###.###.#.###.###.#.#######  \n" +
      "  #...#.......#.#...#...#.............#...#  \n" +
      "  #.#########.#######.#.#######.#######.###  \n" +
      "  #...#.#    F       R I       Z    #.#.#.#  \n" +
      "  #.###.#    D       E C       H    #.#.#.#  \n" +
      "  #.#...#                           #...#.#  \n" +
      "  #.###.#                           #.###.#  \n" +
      "  #.#....OA                       WB..#.#..ZH\n" +
      "  #.###.#                           #.#.#.#  \n" +
      "CJ......#                           #.....#  \n" +
      "  #######                           #######  \n" +
      "  #.#....CK                         #......IC\n" +
      "  #.###.#                           #.###.#  \n" +
      "  #.....#                           #...#.#  \n" +
      "  ###.###                           #.#.#.#  \n" +
      "XF....#.#                         RF..#.#.#  \n" +
      "  #####.#                           #######  \n" +
      "  #......CJ                       NM..#...#  \n" +
      "  ###.#.#                           #.###.#  \n" +
      "RE....#.#                           #......RF\n" +
      "  ###.###        X   X       L      #.#.#.#  \n" +
      "  #.....#        F   Q       P      #.#.#.#  \n" +
      "  ###.###########.###.#######.#########.###  \n" +
      "  #.....#...#.....#.......#...#.....#.#...#  \n" +
      "  #####.#.###.#######.#######.###.###.#.#.#  \n" +
      "  #.......#.......#.#.#.#.#...#...#...#.#.#  \n" +
      "  #####.###.#####.#.#.#.#.###.###.#.###.###  \n" +
      "  #.......#.....#.#...#...............#...#  \n" +
      "  #############.#.#.###.###################  \n" +
      "               A O F   N                     \n" +
      "               A A D   M                     "

  // format: on

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 23
      problem.solve1(input2) ==> 58
    }

    test("solve2 base cases") {
      problem.solve2(input1) ==> 26
      problem.solve2(input3) ==> 396
    }
  }
}