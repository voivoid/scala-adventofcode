package adventOfCode.problems
package year2018

object problem01 extends baseProblem {

  override def solve1(input: Input): Int = {
    val freqChanges = getFreqChanges(input)
    freqChanges.sum
  }

  override def solve2(input: Input): Int = {
    val freqChanges = getFreqChanges(input).toArray
    val freqChangesCycled = Iterator.continually(freqChanges).flatten

    val freqsHistory = freqChangesCycled.scanLeft((Set.empty[Int], 0)) {
      case ((prevFreqs, currentFreq), change) =>
        (prevFreqs + currentFreq, currentFreq + change)
    }

    val (_, freqTwice) = freqsHistory
      .find { case (prevFreqs, currentFreq) => prevFreqs(currentFreq) }
      .getOrElse(sys.error("No solution"))

    freqTwice
  }

  private def getFreqChanges(input: Input): Iterator[Int] = {
    input.getLines().map(_.toInt)
  }

}
