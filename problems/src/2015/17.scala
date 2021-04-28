package adventOfCode.problems
package year2015

object problem17 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve1(input, defaultVolume)
  }

  def solve1(input: Input, toFill: Int): Int = {
    val containerCombinations = generateFillCombinations(input, toFill)

    containerCombinations.size
  }

  override def solve2(input: Input): Int = {
    solve2(input, defaultVolume)
  }

  def solve2(input: Input, toFill: Int): Int = {
    val containerCombinations = generateFillCombinations(input, toFill)
    val minContainers = containerCombinations.iterator.map(_.size).min

    containerCombinations.count(_.size == minContainers)
  }

  private type Containers = List[Int]

  private def generateFillCombinations(input: Input, toFill: Int): List[List[Int]] = {
    val sortedContainers = input.getLines().map(_.toInt).toList.sorted
    calcFillCombinations(sortedContainers, toFill)
  }

  private def calcFillCombinations(containers: Containers, toFill: Int): List[List[Int]] = (containers, toFill) match {
    case (_, 0)                   => List(List())
    case (Nil, _)                 => List()
    case (c :: _, vol) if c > vol => List()
    case (c :: cs, vol) => {
      val combinations = calcFillCombinations(cs, vol - c)
      combinations.map(c :: _) ++ calcFillCombinations(cs, vol)
    }
  }

  private def defaultVolume = 150

}
