package adventOfCode.problems
package year2017

object problem13 extends baseProblem {

  override def solve1(input: Input): Int = {
    val layersVec = parseLayersVec(input)
    layersVec.indices.map(calcSeverity(_, startSecond = 0, layersVec)).sum
  }

  override def solve2(input: Input): Int = {
    val layersVec = parseLayersVec(input)
    Iterator.from(0).find(isSafePass(_, layersVec)).get
  }

  private type Second = Int
  private type Depth = Int
  private type LayersVec = Vector[Depth]

  private def isCaught(layer: Int, startSecond: Second, layersVec: LayersVec): Boolean = {
    val depth = layersVec(layer)
    if (depth == 0) false
    else {
      val roundTrip = 2 * (depth - 1)
      (startSecond + layer) % roundTrip == 0
    }
  }

  private def calcSeverity(layer: Int, startSecond: Second, layersVec: LayersVec): Int = {
    if (!isCaught(layer, startSecond, layersVec)) 0
    else {
      val depth = layersVec(layer)
      layer * depth
    }
  }

  private def parseLayersVec(input: Input): LayersVec = {
    val layersMap = input
      .getLines()
      .map(s => {
        val Array(n, depth) = s.split(": ")
        n.toInt -> depth.toInt
      })
      .toMap

    (0 to layersMap.keys.max).map(layersMap.getOrElse(_, 0)).toVector
  }

  private def isSafePass(startSecond: Int, layersVec: LayersVec): Boolean = {
    !layersVec.indices.exists(isCaught(_, startSecond, layersVec))
  }

}
