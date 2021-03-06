package adventOfCode.problems
package year2018

import adventOfCode.utils.geo
import geo.{Height, Point, Rect, RectDims, Width}

import scala.collection.mutable.ListBuffer

object problem03 extends baseProblem {

  override def solve1(input: Input): Int = {
    val claims = parseClaims(input)

    val overlappedArea = foldOverlappedClaims[Int](claims, initialArea, 0, _ => 1, _ + _)
    overlappedArea
  }

  override def solve2(input: Input): ClaimId = {
    val claims = parseClaims(input)
    val overlappedClaims = foldOverlappedClaims[Set[ClaimId]](claims, initialArea, Set.empty, _.map(_.id).toSet, _ ++ _)

    val nonOverlappedClaims = claims.iterator.map(_.id).toSet -- overlappedClaims
    assert(nonOverlappedClaims.size == 1)

    nonOverlappedClaims.head
  }

  private type Rect = geo.Rect[Int]
  private type ClaimId = Int
  private[problems] case class Claim(id: ClaimId, area: Rect)

  private def parseClaims(input: Input): List[Claim] = input.getLines().map(parseClaim).toList

  private def foldOverlappedClaims[A](
    claims: List[Claim],
    totalArea: Rect,
    nonOverlappedValue: A,
    makeOverlappedValue: List[Claim] => A,
    foldOp: (A, A) => A
  ): A =
    (claims, totalArea) match {
      case (Nil, _)                  => nonOverlappedValue
      case (List(_), RectDims(1, 1)) => nonOverlappedValue
      case (cs, RectDims(1, 1))      => makeOverlappedValue(cs)
      case _ => {
        val lesserAreas = divideRect(totalArea).iterator
        lesserAreas
          .map(area => {
            val areaClaims = claims.filter(_.area.intersects(area))
            foldOverlappedClaims[A](areaClaims, area, nonOverlappedValue, makeOverlappedValue, foldOp)
          })
          .reduceLeft(foldOp)
      }
    }

  private[problems] def parseClaim(line: String): Claim = {
    import fastparse._
    import SingleLineWhitespace._
    import adventOfCode.utils.parse.{num, parseValue}

    def id[_: P] = P("#" ~ num)
    def edge[_: P] = P("@" ~ num ~ "," ~ num)
    def dims[_: P] = P(":" ~ num ~ "x" ~ num)
    def parser[_: P] = (id ~ edge ~ dims).map { case (id, (x, y), (width, height)) =>
      Claim(id, Rect(Point(x, y), Point(x + width - 1, y + height - 1)))
    }

    parseValue(line, parser(_))
  }

  private def initialArea = Rect(Point(0, 0), Point(1000, 1000))

  private[problems] def divideRect(rect: Rect): List[Rect] = {
    import scala.math.Integral.Implicits._
    assert(rect.width > 1 || rect.height > 1)

    val Rect(Point(x1, y1), _) = rect

    val (w, wr) = rect.width /% 2
    val (h, hr) = rect.height /% 2

    val leftTop = Rect(Point(x1, y1), Width(w + wr), Height(h + hr))
    val lb = ListBuffer(leftTop)

    if (h > 0) {
      val leftBottom = Rect(Point(x1, y1 + h + hr), Width(w + wr), Height(h))
      lb.append(leftBottom)
    }

    if (w > 0) {
      val rightTop = Rect(Point(x1 + w + wr, y1), Width(w), Height(h + hr))
      lb.append(rightTop)
    }

    if (h > 0 && w > 0) {
      val rightBottom = Rect(Point(x1 + w + wr, y1 + h + hr), Width(w), Height(h))
      lb.append(rightBottom)
    }

    lb.result()
  }

}
