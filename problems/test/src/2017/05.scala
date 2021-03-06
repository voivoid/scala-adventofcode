package adventOfCode.problems.tests

object year2017_05 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem05 => problem}

  def input = """0
                |3
                |0
                |1
                |-3""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 5
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 10
    }
  }
}
