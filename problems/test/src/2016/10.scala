package adventOfCode.problems.tests

object year2016_10 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem10 => problem}

  def input = """value 5 goes to bot 2
                |bot 2 gives low to bot 1 and high to bot 0
                |value 3 goes to bot 1
                |bot 1 gives low to output 1 and high to bot 0
                |bot 0 gives low to output 2 and high to output 0
                |value 2 goes to bot 2""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input, 5, 2) ==> 2
    }

    test("impl tests") {
      import problem._

      assertMatch(parseInstruction("value 5 goes to bot 2")) { case InputInstruction(5, BotDestination(2)) =>
      }

      assertMatch(parseInstruction("bot 2 gives low to bot 1 and high to bot 0")) {
        case BotInstruction(2, BotDestination(1), BotDestination(0)) =>
      }

      assertMatch(parseInstruction("bot 0 gives low to output 2 and high to output 0")) {
        case BotInstruction(0, OutputDestination(2), OutputDestination(0)) =>
      }
    }
  }

}
