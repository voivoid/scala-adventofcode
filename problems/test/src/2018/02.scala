package adventOfCode.problems.tests

object year2018_02 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem02 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab") ==> 12
    }

    test("solve2 base cases") {
      problem.solve2("abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz") ==> "fgij"
    }
  }
}
