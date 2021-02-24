package adventOfCode.problems.tests

object year2017_03 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem03 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("1") ==> 0
      problem.solve1("12") ==> 3
      problem.solve1("23") ==> 2
      problem.solve1("1024") ==> 31
    }

    test("solve2 base cases") {
      problem.solve2("1") ==> 2
      problem.solve2("2") ==> 4
      problem.solve2("122") ==> 133
      problem.solve2("747") ==> 806
    }
  }
}
