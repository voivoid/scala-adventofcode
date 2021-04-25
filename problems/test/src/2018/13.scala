package adventOfCode.problems.tests

object year2018_13 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem13 => problem}

  // format: off
  def input1 = (
    raw"/->-\        @" +
    raw"|   |  /----\@" +
    raw"| /-+--+-\  |@" +
    raw"| | |  | v  |@" +
    raw"\-+-/  \-+--/@" +
    raw"  \------/   ").replace('@', '\n')

  def input2 = (
    raw"/>-<\  @" +
    raw"|   |  @" +
    raw"| /<+-\@" +
    raw"| | | v@" +
    raw"\>+</ |@" +
    raw"  |   ^@" +
    raw"  \<->/").replace('@', '\n')
  // format: on

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> "7,3"
    }

    test("solve2 base cases") {
      problem.solve2(input2) ==> "6,4"
    }
  }
}
