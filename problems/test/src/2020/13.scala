package adventOfCode.problems.tests

object year2020_13 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem13 => problem}

  def input = """939
                |7,13,x,x,59,x,31,19""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 295
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 1068781L
      problem.solve2("0\n17,x,13,19") ==> 3417L
      problem.solve2("0\n67,7,59,61") ==> 754018L
      problem.solve2("0\n67,x,7,59,61") ==> 779210L
      problem.solve2("0\n67,7,x,59,61") ==> 1261476L
      problem.solve2("0\n1789,37,47,1889") ==> 1202161486L
    }

  }
}
