package adventOfCode.problems.tests

object year2015_05 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem05 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("ugknbfddgicrmopn") ==> 1
      problem.solve1("aaa") ==> 1
      problem.solve1("jchzalrnumimnmhp") ==> 0
      problem.solve1("haegwjzuvuyypxyu") ==> 0
      problem.solve1("dvszwmarrgswjxmb") ==> 0
    }

    test("solve2 base cases") {
      problem.solve2("qjhvhtzxzqqjkmpb") ==> 1
      problem.solve2("xxyxx") ==> 1
      problem.solve2("uurcxstgmygtbstg") ==> 0
      problem.solve2("ieodomkazucvgmuy") ==> 0
    }
  }
}
