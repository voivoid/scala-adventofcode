package adventOfCode.problems.tests

object year2016_04 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem04 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("aaaaa-bbb-z-y-x-123[abxyz]") ==> 123
      problem.solve1("a-b-c-d-e-f-g-h-987[abcde]") ==> 987
      problem.solve1("not-a-real-room-404[oarel]") ==> 404
      problem.solve1("totally-real-room-200[decoy]") ==> 0
    }

    test("impl tests") {
      import problem._
      assertMatch(parseRoom("aaaaa-bbb-z-y-x-123[abxyz]")) { case Room("aaaaa-bbb-z-y-x", 123, "abxyz") => }
    }
  }
}
