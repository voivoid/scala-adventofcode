package adventOfCode.problems.tests

object year2015_12 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem12 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("[1,2,3]") ==> 6
      problem.solve1("{\"a\":2,\"b\":4}") ==> 6
      problem.solve1("[[[3]]]") ==> 3
      problem.solve1("{\"a\":{\"b\":4},\"c\":-1}") ==> 3
      problem.solve1("{\"a\":[-1,1]}") ==> 0
      problem.solve1("[-1,{\"a\":1}]") ==> 0
      problem.solve1("[]") ==> 0
      problem.solve1("{}") ==> 0
    }

    test("solve2 base cases") {
      problem.solve2("[1,2,3]") ==> 6
      problem.solve2("[1,{\"c\":\"red\",\"b\":2},3]") ==> 4
      problem.solve2("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") ==> 0
      problem.solve2("[1,\"red\",5]") ==> 6

    }
  }
}
