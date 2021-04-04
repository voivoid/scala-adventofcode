package adventOfCode.problems
package year2017

object problem11 extends baseProblem {

  override def solve1(input: Input): Int = {
    val steps = parseSteps(input)
    val initialState = State(0, 0)

    val finalState = steps.foldLeft(initialState)(runStep)
    calcDistanceFromStart(finalState)
  }

  override def solve2(input: Input): Int = {
    val steps = parseSteps(input)
    val initialState = State(0, 0)

    val states = steps.scanLeft(initialState)(runStep)
    states.map(calcDistanceFromStart).max
  }

  private def calcDistanceFromStart(state: State): Int = {
    import scala.math.Integral.Implicits._

    val x = state.horizontal.abs
    val y = state.vertical.abs

    if (y > x) {
      val dy = y - x
      val (yDirectSteps, yDiagStep) = dy /% 2
      x + yDirectSteps + yDiagStep
    } else x
  }

  private def parseSteps(input: Input) = {
    import adventOfCode.utils.algorithms.IteratorSplit
    input.splitBy(',')
  }

  private def runStep(state: State, step: String): State = {
    val hor = state.horizontal
    val ver = state.vertical

    step match {
      case "n"  => State(hor, ver + 2)
      case "ne" => State(hor + 1, ver + 1)
      case "nw" => State(hor - 1, ver + 1)
      case "s"  => State(hor, ver - 2)
      case "se" => State(hor + 1, ver - 1)
      case "sw" => State(hor - 1, ver - 1)
    }
  }

  private case class State(horizontal: Int, vertical: Int)

}
