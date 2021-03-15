package adventOfCode.problems
package year2017

object problem06 extends baseProblem {

  override def solve1(input: Input): Int = {
    val initialBank = parseBank(input)
    findNextDuplicateInfo(initialBank).index
  }

  override def solve2(input: Input): Int = {
    val initialBank = parseBank(input)
    val firstDuplicate = findNextDuplicateInfo(initialBank).elem
    findNextDuplicateInfo(firstDuplicate).index
  }

  private def parseBank(input: Input): List[Int] = {
    import adventOfCode.utils.algorithms.IteratorSplit
    input.splitBy('\t').map(_.toInt).toList
  }

  private def findNextDuplicateInfo(initialBank: Bank) = {
    import adventOfCode.utils.algorithms.IteratorFindFirstDuplicate

    val bankSize = initialBank.size
    Iterator.iterate(initialBank)(redistribute(_, bankSize)).findFirstDuplicate.get
  }

  private type Bank = List[Int]

  private def redistribute(bank: Bank, bankSize: Int): Bank = {
    import scala.math.Integral.Implicits._

    val indexedBank = bank.zipWithIndex
    val (maxBlock, maxBlockIndex) = indexedBank.maxBy(_._1)
    val (increaseForAllBlocks, nextBlocksToIncrement) = maxBlock /% bankSize

    def calcBlockIncrease(idx: Int): Int = {
      val range = (maxBlockIndex + 1) to maxBlockIndex + nextBlocksToIncrement
      val idxToFind = if (idx > maxBlockIndex) idx else idx + bankSize

      if (range.contains(idxToFind)) 1 else 0
    }

    indexedBank.map {
      case (_, index) if index == maxBlockIndex => increaseForAllBlocks
      case (block, index) => {
        block + increaseForAllBlocks + calcBlockIncrease(index)
      }
    }
  }

}
