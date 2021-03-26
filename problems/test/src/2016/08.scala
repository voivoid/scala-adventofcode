package adventOfCode.problems.tests

object year2016_08 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem08 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      val input =
        """rect 3x2
          |rotate column x=1 by 1
          |rotate row y=0 by 4
          |rotate column x=1 by 1""".stripMargin

      problem.solve1(input, 7, 3) ==> 6
    }

    test("impl tests") {
      problem.implTests()
    }
  }
}
