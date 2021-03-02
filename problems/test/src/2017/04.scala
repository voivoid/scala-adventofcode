package adventOfCode.problems.tests

object year2017_04 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem04 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("aa bb cc dd ee") ==> 1
      problem.solve1("aa bb cc dd aa") ==> 0
      problem.solve1("aa bb cc dd aaa") ==> 1
    }

    test("solve2 base cases") {
      problem.solve2("abcde fghij") ==> 1
      problem.solve2("abcde xyz ecdab") ==> 0
      problem.solve2("a ab abc abd abf abj") ==> 1
      problem.solve2("iiii oiii ooii oooi oooo") ==> 1
      problem.solve2("oiii ioii iioi iiio") ==> 0
    }
  }
}
