package adventOfCode.problems.tests

object year2017_21 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem21 => problem}

  def input = """../.# => ##./#../...
                |.#./..#/### => #..#/..../..../#..#""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve(input, 2) ==> 12
    }
  }
}
