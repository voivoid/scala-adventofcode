package adventOfCode.problems.tests

object year2020_11 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem11 => problem}

  def input = """L.LL.LL.LL
                |LLLLLLL.LL
                |L.L.L..L..
                |LLLL.LL.LL
                |L.LL.LL.LL
                |L.LLLLL.LL
                |..L.L.....
                |LLLLLLLLLL
                |L.LLLLLL.L
                |L.LLLLL.LL""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 37
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 26
    }

  }
}
