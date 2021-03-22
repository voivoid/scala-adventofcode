package adventOfCode.problems.tests

object year2018_07 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem07 => problem}

  def input = """Step C must be finished before step A can begin.
                |Step C must be finished before step F can begin.
                |Step A must be finished before step B can begin.
                |Step A must be finished before step D can begin.
                |Step B must be finished before step E can begin.
                |Step D must be finished before step E can begin.
                |Step F must be finished before step E can begin.""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> "CABDFE"
    }

    test("solve2 base cases") {
      problem.solve2(input, maxWorkers = 2, stepBaseTime = 0) ==> 15
    }
  }
}
