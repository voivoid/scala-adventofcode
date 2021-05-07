package adventOfCode.problems.tests

object year2017_25 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem25 => problem}

  def input = """Begin in state A.
                |Perform a diagnostic checksum after 6 steps.
                |
                |In state A:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state B.
                |  If the current value is 1:
                |    - Write the value 0.
                |    - Move one slot to the left.
                |    - Continue with state B.
                |
                |In state B:
                |  If the current value is 0:
                |    - Write the value 1.
                |    - Move one slot to the left.
                |    - Continue with state A.
                |  If the current value is 1:
                |    - Write the value 1.
                |    - Move one slot to the right.
                |    - Continue with state A.""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 3
    }

    test("impl tests") {
      import problem._

      val (startState, steps, rules) = parseRules("""Begin in state A.
                   |Perform a diagnostic checksum after 6 steps.
                   |
                   |In state A:
                   |  If the current value is 0:
                   |    - Write the value 1.
                   |    - Move one slot to the right.
                   |    - Continue with state B.
                   |  If the current value is 1:
                   |    - Write the value 0.
                   |    - Move one slot to the left.
                   |    - Continue with state B.
                   |
                   |In state B:
                   |  If the current value is 0:
                   |    - Write the value 1.
                   |    - Move one slot to the left.
                   |    - Continue with state A.
                   |  If the current value is 1:
                   |    - Write the value 1.
                   |    - Move one slot to the right.
                   |    - Continue with state A.""".stripMargin)

      startState ==> 'A'
      steps ==> 6
      rules.size ==> 2

      assertMatch(rules('A')) { case (Action(1, 1, 'B'), Action(0, -1, 'B')) =>
      }

      assertMatch(rules('B')) { case (Action(1, -1, 'A'), Action(1, 1, 'A')) =>
      }
    }
  }
}
