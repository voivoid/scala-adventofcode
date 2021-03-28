package adventOfCode.problems
package year2018

object problem09 extends baseProblem {

  import adventOfCode.utils.collections.{Zipper, CycledZipper}

  override def solve1(input: Input): Long = {
    val (players, lastMarble) = parseGameParams(input.mkString)
    solve(players, lastMarble)
  }

  override def solve2(input: Input): Long = {
    val (players, lastMarble) = parseGameParams(input.mkString)
    solve(players, lastMarble * 100)
  }

  private def solve(playersNum: Int, lastMarble: Int): Long = {
    val initialMarbles: Zipper[Int] = CycledZipper(0)

    val scoresMap = calcScoresMap(nextMarble = 1, initialMarbles, lastMarble, Map.empty, playersNum)
    scoresMap.values.max
  }

  private type Marbles = Zipper[Int]

  @scala.annotation.tailrec
  private def calcScoresMap(
    nextMarble: Int,
    marbles: Marbles,
    marblesLeft: Int,
    scoresMap: Map[Int, Long],
    playersNum: Int
  ): Map[Int, Long] = {

    if (marblesLeft == 0) {
      scoresMap
    } else {
      val (updatedMarbles, score) = calcNextTurnMarbles(nextMarble, marbles)

      val updatedScoresMap = if (score != 0) {
        val playerIndex = nextMarble % playersNum
        scoresMap.updatedWith(playerIndex)(_.orElse(Some(0L)).map(_ + score))
      } else {
        scoresMap
      }

      calcScoresMap(nextMarble + 1, updatedMarbles, marblesLeft - 1, updatedScoresMap, playersNum)
    }
  }

  private def calcNextTurnMarbles(nextMarble: Int, marbles: Marbles): (Marbles, Int) = {
    if (nextMarble % 23 == 0) {
      val prev7marbles = marbles.prev(7)
      (prev7marbles.removeCurrent, prev7marbles.current + nextMarble)
    } else {
      (marbles.next.next.insert(nextMarble), 0)
    }
  }

  private def parseGameParams(str: String): (Int, Int) = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def parser[_: P] = P(num ~ "players;" ~ "last" ~ "marble" ~ "is" ~ "worth" ~ num ~ "points")

    parseValue(str, parser(_))
  }

}
