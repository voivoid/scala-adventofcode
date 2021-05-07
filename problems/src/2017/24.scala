package adventOfCode.problems
package year2017

object problem24 extends baseProblem {

  import scala.collection.immutable.MultiDict

  override def solve1(input: Input): Int = {
    findStrongestBridge(input)
  }

  override def solve2(input: Input): Int = {
    findLongestAndStrongestBridge(input)
  }

  private def findLongestAndStrongestBridge(input: Input): Int = findMaxLenAndStr(input, lenInc=1)

  private def findStrongestBridge(input: Input): Int = {
    // Since lenInc is 0 all bridges total lengths will be 0.
    // This will cause the algorithm to basically ignore the length and to compare only the strength.
    findMaxLenAndStr(input, lenInc=0)
  }


  private def findMaxLenAndStr(input: Input, lenInc: Int): Int = {
    val componentsMap = parseComponentsMap(input)
    val (maxLen@_, maxStrength) = findLongestAndStrongestBridges(0, 0, 0, componentsMap, lenInc)

    maxStrength
  }

  private def findLongestAndStrongestBridges(from: Int, len: Int, strength: Int, componentsMap: ComponentsMap, lenInc: Int): (Int, Int) = {
    val matched = componentsMap.get(from)
    if (matched.isEmpty) (len, strength)
    else {
      matched
        .map(to => findLongestAndStrongestBridges(to, len + lenInc, strength + from + to, componentsMap - (from -> to) - (to -> from), lenInc))
        .max
    }
  }

  private type ComponentsMap = MultiDict[Int, Int]

  private def parseComponentsMap(input: Input): ComponentsMap = {
    val lines = input.getLines().toVector
    lines
      .flatMap(line => {
        val Array(from, to) = line.split('/')
        val f = from.toInt
        val t = to.toInt
        List(f -> t, t -> f)
      })
      .to(MultiDict)
  }

}
