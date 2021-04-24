package adventOfCode.problems.tests

object year2017_13 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem13 => problem}

  def input = """0: 3
                |1: 2
                |4: 4
                |6: 4""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 24
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 10
    }
  }
}
