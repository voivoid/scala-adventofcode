package adventOfCode.problems.tests

object year2015_11 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem11 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("abcdefgh") ==> "abcdffaa"
      problem.solve1("ghijklmn") ==> "ghjaabcc"

      problem.solve1("aaaaa") ==> "aabcc"
      problem.solve1("aaaaz") ==> "aabcc"
      problem.solve1("aaaza") ==> "aabcc"
      problem.solve1("aaazz") ==> "aabcc"
      problem.solve1("aabaz") ==> "aabcc"
      problem.solve1("aabza") ==> "bbcdd"
      problem.solve1("aabzz") ==> "bbcdd"
      problem.solve1("abaaa") ==> "bbcdd"
      problem.solve1("aacaa") ==> "bbcdd"
      problem.solve1("aaada") ==> "aabcc"
      problem.solve1("aaaad") ==> "aabcc"
      problem.solve1("bazzz") ==> "bbcdd"
      problem.solve1("aabca") ==> "aabcc"
      problem.solve1("aabcb") ==> "aabcc"
      problem.solve1("aabcc") ==> "bbcdd"
      problem.solve1("xyaaa") ==> "aaabcc"
      problem.solve1("xxxxx") ==> "xxyzz"
      problem.solve1("yyyyy") ==> "aaabcc"
      problem.solve1("zzzzz") ==> "aaabcc"

      problem.solve1("ggggg") ==> "ppqrr"
      problem.solve1("xxzzzz") ==> "xyzzaa"

      problem.solve1("a") ==> "aabcc"
      problem.solve1("zzzz") ==> "aabcc"
    }
  }
}
