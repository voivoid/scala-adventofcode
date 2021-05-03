package adventOfCode.problems
package year2017

object problem17 extends baseProblem {

  import adventOfCode.utils.collections.{Zipper, CycledZipper}

  override def solve1(input: Input): Int = {
    val stepsPerTurn = input.mkString.toInt
    val initialState = State(CycledZipper[Int](List(0)).next, 1)

    import adventOfCode.utils.algorithms.IteratorLast
    val finalState = Iterator.iterate(initialState, 2017 + 1)(nextState(_, stepsPerTurn)).last

    finalState.numbers.next.current
  }

  override def solve2(input: Input): Long = {
    val stepsPerTurn = input.mkString.toInt
    val turns = 50000000L

    findX(stepsPerTurn, turns)
  }

  private case class State(numbers: Zipper[Int], size: Int)

  private def nextState(state: State, stepsPerTurn: Int): State = {
    import state._

    val steps = stepsPerTurn % size
    val nextNums = numbers.next(steps).append(size)

    State(nextNums, size + 1)
  }

  private def findX(stepsPerTurn: Int, turnsTotal: Long): Long = {
    @scala.annotation.tailrec
    def calc(currentPos: Long, turn: Long, currentValue: Long): Long = {
      if (turn != turnsTotal) {
        val steps = stepsPerTurn % turn
        val nextPos = (currentPos + steps) % turn
        val nextValue = if (nextPos == 0) turn else currentValue
        calc(nextPos + 1, turn + 1, nextValue)
      } else currentValue
    }

    calc(0, 1, 0)
  }

}
