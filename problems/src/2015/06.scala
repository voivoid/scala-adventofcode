package adventOfCode.problems
package year2015

import adventOfCode.utils.geo.{Point, Rect}

object problem06 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, lightFunction1)
  }

  override def solve2(input: Input): Int = {
    solve(input, lightFunction2)
  }

  private def solve(input: Input, lightFunction: Switch => (Light => Light)) = {
    val instructions = input.getLines().map(parseInstruction)
    val initGrid = makeInitGrid

    val finalGrid = instructions.foldLeft(initGrid)(runInstruction(_, _, lightFunction))

    finalGrid.iterator.flatten.sum
  }

  private type Light = Int
  private type Grid = Array[Array[Light]]
  private def GridSide = 1000
  private def makeInitGrid: Grid = Array.ofDim[Light](GridSide, GridSide)

  private sealed trait Switch
  private case object On extends Switch
  private case object Off extends Switch
  private case object Toggle extends Switch

  private case class Instruction(switch: Switch, rect: Rect[Int])

  private def lightFunction1(switch: Switch) = {
    switch match {
      case On     => (_: Int) => 1
      case Off    => (_: Int) => 0
      case Toggle => (n: Int) => (n + 1) % 2
    }
  }

  private def lightFunction2(switch: Switch) = {
    switch match {
      case On     => (n: Int) => n + 1
      case Off    => (n: Int) => (n - 1) max 0
      case Toggle => (n: Int) => n + 2
    }
  }

  private def runInstruction(grid: Grid, instruction: Instruction, selectLightFunction: Switch => (Light => Light)): Grid = {
    val calcLight = selectLightFunction(instruction.switch)
    val Rect(Point(left, top), Point(right, bottom)) = instruction.rect

    for {
      x <- left to right
      y <- top to bottom
      light = grid(y)(x)
    } grid(y)(x) = calcLight(light)

    grid
  }

  private def parseInstruction(str: String): Instruction = {
    import fastparse._
    import SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def switch[_: P] = P(P("turn on").map(_ => On) | P("turn off").map(_ => Off) | P("toggle").map(_ => Toggle))
    def point[_: P] = P(num ~ "," ~ num).map { case (x, y) => Point(x, y) }
    def rect[_: P] = P(point ~ "through" ~ point).map { case (p1, p2) => Rect(p1, p2) }

    def parser[_: P] = P(switch ~ rect).map(Instruction tupled _)

    parseValue(str, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

    assertMatch(parseInstruction("turn on 0,0 through 999,999")) { case Instruction(On, Rect(Point(0, 0), Point(999, 999))) => }
  }

}
