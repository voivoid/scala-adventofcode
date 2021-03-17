package adventOfCode.problems.tests

object year2017_07 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem07 => problem}

  def input = """pbga (66)
                |xhth (57)
                |ebii (61)
                |havc (66)
                |ktlj (57)
                |fwft (72) -> ktlj, cntj, xhth
                |qoyq (66)
                |padx (45) -> pbga, havc, qoyq
                |tknk (41) -> ugml, padx, fwft
                |jptl (61)
                |ugml (68) -> gyxo, ebii, jptl
                |gyxo (61)
                |cntj (57)""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> "tknk"
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 60
    }

    test("impl tests") {
      problem.implTests()
    }
  }
}
