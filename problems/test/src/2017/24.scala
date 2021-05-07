package adventOfCode.problems.tests

object year2017_24 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem24 => problem}

  def input = """0/2
                |2/2
                |2/3
                |3/4
                |3/5
                |0/1
                |10/1
                |9/10""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 31
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 19
    }
  }
}
