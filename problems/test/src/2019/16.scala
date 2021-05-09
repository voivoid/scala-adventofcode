package adventOfCode.problems.tests

object year2019_16 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem16 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("12345678", 4) ==> "01029498"
      problem.solve1("80871224585914546619083218645595", 100) ==> "24176176"
      problem.solve1("19617804207202209144916044189917", 100) ==> "73745418"
      problem.solve1("69317163492948606335995924319873", 100) ==> "52432133"
    }
  }
}
