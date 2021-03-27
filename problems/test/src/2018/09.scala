package adventOfCode.problems.tests

object year2018_09 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem09 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("9 players; last marble is worth 25 points") ==> 32
      problem.solve1("10 players; last marble is worth 1618 points") ==> 8317
      problem.solve1("13 players; last marble is worth 7999 points") ==> 146373
      problem.solve1("17 players; last marble is worth 1104 points") ==> 2764
      problem.solve1("21 players; last marble is worth 6111 points") ==> 54718
      problem.solve1("30 players; last marble is worth 5807 points") ==> 37305
    }
  }
}
