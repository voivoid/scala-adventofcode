package adventOfCode.problems.tests

object year2020_06 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem06 => problem}

  def input = """abc
                |
                |a
                |b
                |c
                |
                |ab
                |ac
                |
                |a
                |a
                |a
                |a
                |
                |b""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 11
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 6
    }

    test("impl tests") {
      problem.implTests()
    }
  }
}
