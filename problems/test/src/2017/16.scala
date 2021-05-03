package adventOfCode.problems.tests

object year2017_16 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem16 => problem}

  def input = "s1,x3/4,pe/b"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input, 5) ==> "baedc"
    }
  }
}
