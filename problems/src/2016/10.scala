package adventOfCode.problems
package year2016

object problem10 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve1(input, 61, 17)
  }

  def solve1(input: Input, val1: Int, val2: Int): Int = {
    val (botsMap, _) = runInstructions(input)

    val valuesToFind = Set(val1, val2)
    val resultBot = botsMap
      .find { _._2 == valuesToFind }
      .getOrElse(sys.error("no solution"))

    resultBot._1
  }

  override def solve2(input: Input): Int = {
    val (_, outputsMap) = runInstructions(input)
    Seq(0, 1, 2).map(outputsMap).product
  }

  private def runInstructions(input: Input): (BotValues, OutputValues) = {
    val (inputInstructions, botInsructions) = input.getLines().map(parseInstruction).toList.partitionMap {
      case ii: InputInstruction => Left(ii)
      case bi: BotInstruction   => Right(bi)
    }

    val botInstructionsMap = botInsructions.iterator
      .map(i => (i: @unchecked) match { case instruction @ BotInstruction(index, _, _) => index -> instruction })
      .toMap

    runInstructions(inputInstructions, botInstructionsMap, Map.empty, Map.empty)
  }

  @scala.annotation.tailrec
  private def runInstructions(
    readyInstructions: List[Instruction],
    botInstructions: BotInstructions,
    botsMap: BotValues,
    outputsMap: OutputValues
  ): (BotValues, OutputValues) = {

    readyInstructions match {
      case Nil => (botsMap, outputsMap)

      case BotInstruction(bot, lowDestination, highDestination) :: restInstructions => {
        val values = botsMap(bot)
        assert(values.size == 2)

        val (low, high) = (values.min, values.max)

        val ii1 = InputInstruction(low, lowDestination)
        val ii2 = InputInstruction(high, highDestination)

        runInstructions(ii1 :: ii2 :: restInstructions, botInstructions, botsMap, outputsMap)
      }

      case InputInstruction(value, destination) :: restInstructions =>
        destination match {
          case BotDestination(index) => {
            val updatedBotsMap = botsMap.updatedWith(index)(_.orElse(Some(Set.empty[Value])).map(_.incl(value)))

            val updatedReadyInstructions = if (updatedBotsMap(index).size == 2) {
              botInstructions(index) :: restInstructions
            } else {
              restInstructions
            }

            runInstructions(updatedReadyInstructions, botInstructions, updatedBotsMap, outputsMap)
          }

          case OutputDestination(outputBin) => {
            val updatedOutputsMap = outputsMap.updated(outputBin, value)
            runInstructions(restInstructions, botInstructions, botsMap, updatedOutputsMap)
          }
        }
    }
  }

  private type Value = Int
  private type Index = Int
  private type BotInstructions = Map[Index, BotInstruction]
  private type BotValues = Map[Index, Set[Value]]
  private type OutputValues = Map[Index, Value]

  private sealed trait Destination
  private case class BotDestination(bot: Index) extends Destination
  private case class OutputDestination(bin: Index) extends Destination

  private sealed trait Instruction
  private case class InputInstruction(value: Value, to: Destination) extends Instruction
  private case class BotInstruction(bot: Index, lowTo: Destination, highTo: Destination) extends Instruction

  private def parseInstruction(s: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def outputDest[_: P] = P("output" ~ num).map(OutputDestination)
    def botDest[_: P] = P("bot" ~ num).map(BotDestination)
    def dest[_: P]: P[Destination] = P(outputDest | botDest)
    def input[_: P] = P("value" ~ num ~ "goes" ~ "to" ~ dest).map(InputInstruction tupled _)
    def bot[_: P] = P("bot" ~ num ~ "gives" ~ "low" ~ "to" ~ dest ~ "and" ~ "high" ~ "to" ~ dest).map(BotInstruction tupled _)
    def parser[_: P]: P[Instruction] = P(input | bot)

    parseValue(s, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

    assertMatch(parseInstruction("value 5 goes to bot 2")) { case InputInstruction(5, BotDestination(2)) =>
    }

    assertMatch(parseInstruction("bot 2 gives low to bot 1 and high to bot 0")) {
      case BotInstruction(2, BotDestination(1), BotDestination(0)) =>
    }

    assertMatch(parseInstruction("bot 0 gives low to output 2 and high to output 0")) {
      case BotInstruction(0, OutputDestination(2), OutputDestination(0)) =>
    }
  }

}
