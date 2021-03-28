package adventOfCode.problems.tests

object year2020_09 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem09 => problem}

  def input = """35
                |20
                |15
                |25
                |47
                |40
                |62
                |55
                |65
                |95
                |102
                |117
                |150
                |182
                |127
                |219
                |299
                |277
                |309
                |576""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input, 5) ==> 127
    }

    test("solve2 base cases") {
      problem.solve2(input, 5) ==> 62
    }

  }
}
