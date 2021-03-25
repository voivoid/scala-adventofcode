package adventOfCode.problems.tests

object year2020_08 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem08 => problem}

  def input = """nop +0
                |acc +1
                |jmp +4
                |acc +3
                |jmp -3
                |acc -99
                |acc +1
                |jmp -4
                |acc +6""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 5
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 8
    }

    test("impl tests") {
      problem.implTests()
    }
  }
}
