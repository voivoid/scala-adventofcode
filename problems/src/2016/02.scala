package adventOfCode.problems
package year2016

object problem02 extends baseProblem {

  override def solve1(input: Input): String = {
    solve(input, keypad1)
  }

  override def solve2(input: Input): String = {
    solve(input, keypad2)
  }

  private def solve(input: Input, keypad: String): String = {
    val keypadMap = makeKeypadMap(keypad)
    val keysPressed = input.getLines().scanLeft('5')(runInsructions(keypadMap)).drop(1)

    keysPressed.mkString
  }


  private type Key = Char
  private type Instruction = Char
  private type KeypadMap = Map[(Key, Instruction), Key]

  private def makeKeypadMap(keypad: String): KeypadMap = {
    import adventOfCode.utils.algorithms.SlidingTuple

    val lines = keypad.split('\n').map(_.toArray)

    def makeMap(lines: Array[Array[Char]], next: Instruction, prev: Instruction): KeypadMap = {
      val keyValues = for {
        line <- lines
        (key1, key2) <- line.iterator.sliding2
        if !key1.isWhitespace && !key2.isWhitespace
        keyValue <- Seq( ((key1,next), key2), ((key2, prev), key1))
      } yield keyValue

      keyValues.toMap
    }

    makeMap(lines, 'R', 'L') ++ makeMap(lines.transpose, 'D', 'U')
  }

  private def runInsructions(keypadMap: KeypadMap)(start: Key, instructions: String): Key = {
    instructions.foldLeft(start){
      case (currentKey, instruction) => keypadMap.getOrElse((currentKey, instruction), currentKey)
    }
  }

  private def keypad1 =
  """123
    |456
    |789""".stripMargin

  private def keypad2 =
  "  1  \n" +
  " 234 \n" +
  "56789\n" +
  " ABC \n" +
  "  D  "

}
