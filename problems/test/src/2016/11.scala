package adventOfCode.problems.tests

object year2016_11 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem11 => problem}

  def input = """The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
                |The second floor contains a hydrogen generator.
                |The third floor contains a lithium generator.
                |The fourth floor contains nothing relevant.""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 11
    }

    test("impl tests") {
      problem.implTests()
    }
  }

}
