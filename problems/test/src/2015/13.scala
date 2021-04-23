package adventOfCode.problems.tests

object year2015_13 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem13 => problem}

  def input = """Alice would gain 54 happiness units by sitting next to Bob.
                |Alice would lose 79 happiness units by sitting next to Carol.
                |Alice would lose 2 happiness units by sitting next to David.
                |Bob would gain 83 happiness units by sitting next to Alice.
                |Bob would lose 7 happiness units by sitting next to Carol.
                |Bob would lose 63 happiness units by sitting next to David.
                |Carol would lose 62 happiness units by sitting next to Alice.
                |Carol would gain 60 happiness units by sitting next to Bob.
                |Carol would gain 55 happiness units by sitting next to David.
                |David would gain 46 happiness units by sitting next to Alice.
                |David would lose 7 happiness units by sitting next to Bob.
                |David would gain 41 happiness units by sitting next to Carol.""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 330
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 286
    }

    test("impl tests") {
      import problem._
      assertMatch(parseSeat("Alice would gain 54 happiness units by sitting next to Bob.")) { case SeatInfo("Alice", 54, "Bob") =>
      }

      assertMatch(parseSeat("Bob would lose 7 happiness units by sitting next to Carol.")) { case SeatInfo("Bob", -7, "Carol") =>
      }
    }
  }
}
