package adventOfCode.problems.tests

object year2017_09 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem09 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("{}") ==> 1
      problem.solve1("{{{}}}") ==> 6
      problem.solve1("{{},{}}") ==> 5
      problem.solve1("{{{},{},{{}}}}") ==> 16
      problem.solve1("{<a>,<a>,<a>,<a>}") ==> 1
      problem.solve1("{{<ab>},{<ab>},{<ab>},{<ab>}}") ==> 9
      problem.solve1("{{<!!>},{<!!>},{<!!>},{<!!>}}") ==> 9
      problem.solve1("{{<a!>},{<a!>},{<a!>},{<ab>}}") ==> 3
    }

    test("solve2 base cases") {
      problem.solve2("<>") ==> 0
      problem.solve2("<random characters>") ==> 17
      problem.solve2("<<<<>") ==> 3
      problem.solve2("<{!>}>") ==> 2
      problem.solve2("<!!>") ==> 0
      problem.solve2("<!!!>>") ==> 0
      problem.solve2("<{o\"i!a,<{i<a>") ==> 10
    }
  }
}
