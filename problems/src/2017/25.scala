package adventOfCode.problems
package year2017

object problem25 extends baseProblem {

  override def solve1(input: Input): Int = {
    val (initialState, stepsToRun, rules) = parseRules(input.mkString)
    val initialSM = StateMachine(Array.fill(100)(0), cursor = 50, initialState)

    import adventOfCode.utils.algorithms.IteratorLast
    val finalState = Iterator.iterate(initialSM, stepsToRun + 1)(runStep(rules, _)).last

    finalState.slots.count(_ == 1)
  }

  override def solve2(input: Input): Int = {
    0
  }

  private type Slots = Array[Int]
  private[problems] type State = Char
  private[problems] type Rules = Map[State, (Action, Action)]

  private case class StateMachine(slots: Slots, cursor: Int, state: Char)
  private[problems] case class Action(valueToWrite: Int, cursorOffset: Int, nextState: State)

  private def runStep(rules: Rules, sm: StateMachine): StateMachine = {
    import sm._

    val (a0, a1) = rules(state)
    val currentVal = slots(cursor)
    val action = if (currentVal == 0) a0 else a1

    slots(cursor) = action.valueToWrite

    val (nextSlots, nextCursor) = {
      val nextCursor = cursor + action.cursorOffset
      if (slots.indices.contains(nextCursor)) (slots, nextCursor)
      else inflate(slots, nextCursor)
    }

    StateMachine(nextSlots, nextCursor, action.nextState)
  }

  private[problems] def parseRules(s: String): (State, Int, Rules) = {
    import fastparse._
    import fastparse.MultiLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, alpha, num}
    import Function.const

    def startState[_: P] = P("Begin" ~ "in" ~ "state" ~ alpha ~ ".")
    def steps[_: P] = P("Perform" ~ "a" ~ "diagnostic" ~ "checksum" ~ "after" ~ num ~ "steps.")
    def dir[_: P] = P("right").map(const(1)) | P("left").map(const(-1))

    def valueToWrite[_: P] = P("-" ~ "Write" ~ "the" ~ "value" ~ num ~ ".")
    def slotToMove[_: P] = P("-" ~ "Move" ~ "one" ~ "slot" ~ "to" ~ "the" ~ dir ~ ".")
    def nextState[_: P] = P("-" ~ "Continue" ~ "with" ~ "state" ~ alpha ~ ".")

    def ifValue[_: P] = P("If" ~ "the" ~ "current" ~ "value" ~ "is" ~ num ~ ":") ~ valueToWrite ~ slotToMove ~ nextState
    def inState[_: P] = P("In" ~ "state" ~ alpha ~ ":")

    def state[_: P] = P(inState ~ ifValue ~ ifValue)
    def states[_: P] = P(state.rep(1))

    def parser[_: P] = P(startState ~ steps ~ states)

    val (parsedStartState, parsedSteps, parsedRules) = parseValue(s, parser(_))

    val rules = parsedRules.map {
      case (state, (0, toWrite0, move0, nextState0), (1, toWrite1, move1, nextState1)) => {
        val a1 = Action(toWrite0, move0, nextState0)
        val a2 = Action(toWrite1, move1, nextState1)
        (state, (a1, a2))
      }
    }.toMap

    (parsedStartState, parsedSteps, rules)
  }

  private def inflate(slots: Slots, cursor: Int): (Slots, Int) = {
    val width = slots.size

    val nextSlots = Array.tabulate(width * 3) {
      case i if i >= width && i < (width * 2) => slots(i - width)
      case _                                  => 0
    }

    val nextCursor = cursor + width

    (nextSlots, nextCursor)
  }

}
