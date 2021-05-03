package adventOfCode.problems
package year2020

object problem03 extends baseProblem {

  override def solve1(input: Input): Int = {
    val (lines, lineWidth) = parseInput(input)

    calcTrees(lines, lineWidth, 3, 1)
  }

  override def solve2(input: Input): Long = {
    val (lines, lineWidth) = parseInput(input)

    val slopes = Iterator((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    val treesPerSlope = slopes.map {
      case (right, down) => {
        calcTrees(lines, lineWidth, right, down)
      }
    }

    treesPerSlope.map(_.toLong).product
  }

  private def parseInput(input: Input): (List[String], Int) = {
    val lines = input.getLines().toList
    val lineWidth = lines.head.size
    (lines, lineWidth)
  }

  private def calcTrees(lines: List[String], lineWidth: Int, right: Int, down: Int): Int = {
    val xCoords = Iterator.iterate(0)(x => (x + right) % lineWidth)
    val filteredLines = lines.zipWithIndex.filter { case (_, lineIndex) => lineIndex % down == 0 }.map(_._1)

    xCoords.zip(filteredLines).count { case (x, line) =>
      line(x) == '#'
    }
  }

}
