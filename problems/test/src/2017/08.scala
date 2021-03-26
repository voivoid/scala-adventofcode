package adventOfCode.problems.tests

object year2017_08 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem08 => problem}

  def input = """b inc 5 if a > 1
                |a inc 1 if b < 5
                |c dec -10 if a >= 1
                |c inc -20 if c == 10""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 1
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 10
    }

    test("impl tests") {
      problem.implTests()
    }
  }
}
