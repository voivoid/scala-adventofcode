package adventOfCode.problems
package year2017

object problem05 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, calcNextOffset1)
  }

  override def solve2(input: Input): Int = {
    solve(input, calcNextOffset2)
  }

  private def solve(input: Input, calcNextOffset: Int => Int): Int = {
    val initialOffsets = input.getLines().map(_.toInt).toVector
    val initialIndex = 0
    val maxOffset = initialOffsets.size

    val offsetsHistory = Iterator.iterate((initialOffsets, initialIndex)){ case (offsets, index) => {
      val offset = offsets(index)
      val updatedOffset = calcNextOffset(offset)
      (offsets.updated(index, updatedOffset), index + offset)
    } }

    offsetsHistory.takeWhile{ case (_, offset) => offset >= 0 && offset < maxOffset}.size
  }

  private def calcNextOffset1(offset: Int): Int = offset + 1
  private def calcNextOffset2(offset: Int): Int = if(offset >= 3 ) offset - 1 else offset + 1

  }
