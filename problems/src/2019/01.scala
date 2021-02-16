package adventOfCode.problems
package year2019

object problem01 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, calcFuelSimple)
  }

  override def solve2(input: Input): Int = {
    solve(input, calcFuelTotal)
  }

  private def solve(input: Input, calcFuel: Int => Int): Int = {
    input
      .getLines()
      .map(massStr => calcFuel(massStr.toInt))
      .sum
  }

  private def calcFuelSimple(mass: Int): Int =
    mass / 3 - 2

  private def calcFuelTotal(mass: Int): Int = {
    Iterator
      .iterate(mass)(calcFuelSimple)
      .drop(1)
      .takeWhile(_ > 0)
      .sum
  }
}
