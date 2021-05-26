package adventOfCode.problems
package year2019

object problem19 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Int = {
    val initialMachine = run(parseMemory(input.mkString))

    val side = 50

    val affected = for {
      x <- (0 until side).iterator
      y <- (0 until side).iterator
    } yield isAffected(x, y, initialMachine)

    affected.count(_ == true)
  }

  override def solve2(input: Input): Int = {
    val initialMachine = run(parseMemory(input.mkString))
    val squareSide = 100 - 1

    def find(xFrom: Int, y: Int, machine: Machine): (Int, Int) = {
      val x = Iterator.from(xFrom).find(isAffected(_, y, machine)).get
      if (isAffected(x + squareSide, y - squareSide, machine)) (x, y - squareSide)
      else find(x, y + 1, machine)
    }

    val (x, y) = find(0, squareSide, initialMachine)
    x * 10000 + y
  }

  private def isAffected(x: Int, y: Int, machine: Machine): Boolean = {
    resumeMachine(machine, List(x.toLong, y.toLong)).output.head == 1
  }

}
