package adventOfCode.problems.tests

object year2018_03 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem03 => problem}

  val input = """#1 @ 1,3: 4x4
                |#2 @ 3,1: 4x4
                |#3 @ 5,5: 2x2""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 4
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 3
    }

    test("impl tests") {
      problem.implTests()
    }
  }
}
