package adventOfCode.problems.tests

object year2020_02 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem02 => problem}

  val input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 2
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 1
    }

    test("impl tests") {
      import problem._

      assertMatch(parseEntry("1-3 a: abcd")) { case Entry(1, 3, 'a', "abcd") => }
    }
  }
}
