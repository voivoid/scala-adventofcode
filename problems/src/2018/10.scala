package adventOfCode.problems
package year2018

object problem10 extends baseProblem {

  override def solve1(input: Input): String = {
    val points = input.getLines().map(parsePoint).toVector
    val messagePoints = pointsStatesIterator(points).find(checkMessageIsVisible).get

    val (left, top, width, height) = calcMessageDims(messagePoints)

    val messagePointsShiftedToOrigin = messagePoints.iterator.map { case Point(x, y, _, _) => (x - left, y - top) }

    val pixels = getMessagePixels(messagePointsShiftedToOrigin, width, height)
    adventOfCode.utils.ocr.decodeChars(pixels, width, height, charWidth)
  }

  override def solve2(input: Input): Int = {
    val points = input.getLines().map(parsePoint).toVector
    pointsStatesIterator(points).indexWhere(checkMessageIsVisible(_))
  }

  private def charWidth = 8
  private def charHeight = 10

  private[problems] case class Point(x: Int, y: Int, velx: Int, vely: Int)
  private type Points = Vector[Point]

  private def pointsStatesIterator(initial: Points): Iterator[Points] = {
    Iterator.iterate(initial)(calcNextState)
  }

  private def calcNextState(points: Vector[Point]): Vector[Point] = {
    points.map(calcNextPosition)
  }

  private def calcNextPosition(p: Point): Point = {
    p.copy(x = p.x + p.velx, y = p.y + p.vely)
  }

  private def checkMessageIsVisible(points: Vector[Point]): Boolean = {
    val firstY = points.head.y
    val ys = points.iterator.map(_.y)

    val minmax = ys.scanLeft((firstY, firstY)) { case ((min, max), y) =>
      (min.min(y), max.max(y))
    }

    minmax.forall { case (min, max) => max - min <= charHeight }
  }

  private def getMessagePixels(shiftedPoints: Iterator[(Int, Int)], width: Int, height: Int): IndexedSeq[Char] = {
    val messageGrid = Array.fill(height, width)('0')
    shiftedPoints.foreach { case (x, y) => messageGrid(y)(x) = '1' }

    messageGrid.flatten.toIndexedSeq
  }

  private def calcMessageDims(points: Points): (Int, Int, Int, Int) = {
    import adventOfCode.utils.algorithms.IteratorMinMax

    val (left, right) = points.iterator.map(_.x).minmax
    val width = 1 + 2 + right - left
    assert(width % charWidth == 0)

    val (top, bottom) = points.iterator.map(_.y).minmax
    val height = 1 + bottom - top
    assert(height == charHeight)

    (left, top, width, height)
  }

  private[problems] def parsePoint(s: String): Point = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def pos[_: P] = P("position=<" ~ num ~ "," ~ num ~ ">")
    def vel[_: P] = P("velocity=<" ~ num ~ "," ~ num ~ ">")

    def parser[_: P] = P(pos ~ vel).map { case (x, y, (vx, vy)) => Point(x, y, vx, vy) }

    parseValue(s, parser(_))
  }

}
