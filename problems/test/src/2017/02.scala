package adventOfCode.problems.tests

object year2017_02 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem02 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("5\t1\t9\t5") ==> 8
      problem.solve1("7\t5\t3") ==> 4
      problem.solve1("2\t4\t6\t8") ==> 6
    }

    test("solve2 base cases") {
      problem.solve2("5\t9\t2\t8") ==> 4
      problem.solve2("9\t4\t7\t3") ==> 3
      problem.solve2("3\t8\t6\t5") ==> 2
    }
  }
}
