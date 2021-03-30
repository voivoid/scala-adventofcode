package adventOfCode.problems
package year2016

object problem08 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve1(input, width = 50, height = 6)
  }

  def solve1(input: Input, width: Int, height: Int): Int = {
    val finalGrid = runInstructions(input, width, height)
    finalGrid.iterator.flatten.count(_ == On)
  }

  override def solve2(input: Input): String = {
    val width = 50
    val height = 6
    val finalGrid = runInstructions(input, width, height)

    val pixels = finalGrid.flatten.toIndexedSeq
    adventOfCode.utils.ocr.decodeChars(pixels, width, height, charWidth = 5)
  }

  private def runInstructions(input: Input, width: Int, height: Int): Grid = {
    val instructions = input.getLines().map(parseInstruction)
    val initGrid = Array.fill[Char](height, width)(Off)
    instructions.foldLeft(initGrid)(runInstruction)
  }

  private def runInstruction(grid: Grid, instruction: Instruction): Grid = {
    instruction.run(grid)
  }

  private type Grid = Array[Array[Char]]
  private val On = '1'
  private val Off = '0'

  private sealed trait Instruction {
    def run(grid: Grid): Grid
  }

  private case class Rect(width: Int, height: Int) extends Instruction {
    override def run(grid: Grid): Grid = {
      for {
        x <- 0 until width
        y <- 0 until height
      } grid(y)(x) = On

      grid
    }
  }

  private case class RotateCol(x: Int, by: Int) extends Instruction {
    override def run(grid: Grid): Grid = {
      val height = grid.length
      val colIndices = 0 until height

      val col = for {
        y <- colIndices
      } yield grid(y)(x)

      import adventOfCode.utils.algorithms.IterableCycle
      val rotatedCol = col.cycled.drop(height - by)

      for {
        (y, value) <- colIndices.zip(rotatedCol)
      } grid(y)(x) = value

      grid
    }
  }

  private case class RotateRow(y: Int, by: Int) extends Instruction {
    override def run(grid: Grid): Grid = {
      val width = grid(0).length
      val rowIndices = 0 until width

      val row = for {
        x <- rowIndices
      } yield grid(y)(x)

      import adventOfCode.utils.algorithms.IterableCycle
      val rotatedRow = row.cycled.drop(width - by)

      for {
        (x, value) <- rowIndices.zip(rotatedRow)
      } grid(y)(x) = value

      grid
    }
  }

  private def parseInstruction(str: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def rect[_: P] = P("rect" ~ num ~ "x" ~ num).map(Rect tupled _)
    def rotateArgs[_: P] = P(num ~ "by" ~ num)
    def rotateCol[_: P] = P("column" ~ "x=")
    def rotateRow[_: P] = P("row" ~ "y=")
    def rotate[_: P] = P(
      "rotate" ~
        ((rotateCol ~ rotateArgs).map(RotateCol tupled _) | (rotateRow ~ rotateArgs).map(RotateRow tupled _))
    )
    def parser[_: P] = P(rect | rotate)

    parseValue(str, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

    assertMatch(parseInstruction("rect 3x2")) { case Rect(3, 2) => }
    assertMatch(parseInstruction("rotate column x=1 by 1")) { case RotateCol(1, 1) => }
    assertMatch(parseInstruction("rotate row y=0 by 4")) { case RotateRow(0, 4) => }
  }

}
