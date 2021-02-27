package adventOfCode.utils
package geo

case class Width[A](n: A)
case class Height[A](n: A)

case class Point[A](x: A, y: A)(implicit num: Numeric[A]) {
  import num._

  def move(dx: A, dy: A): Point[A] = {
    Point(plus(x, dx), plus(y, dy))
  }
}

case class Rect[A](leftTop: Point[A], rightBottom: Point[A])(implicit num: Numeric[A]) {
  import num._

  def contains(other: Point[A]): Boolean = {
    val (Point(x1, y1), Point(x2, y2)) = (leftTop, rightBottom)
    lteq(x1, other.x) && lteq(y1, other.y) && gteq(x2, other.x) && gteq(y2, other.y)
  }

  def contains(other: Rect[A]): Boolean = {
    contains(other.leftTop) && contains(other.rightBottom)
  }

  def intersects(other: Rect[A]): Boolean = {
    val Rect(Point(left1, top1), Point(right1, bottom1)) = this
    val Rect(Point(left2, top2), Point(right2, bottom2)) = other

    !(right1 < left2 || right2 < left1 || bottom1 < top2 || bottom2 < top1)
  }

  def width: A = {
    plus(one, abs(minus(rightBottom.x, leftTop.x)))
  }
  def height: A = {
    plus(one, abs(minus(rightBottom.y, leftTop.y)))
  }

  def area: A = times(width, height)
}

object Rect {
  def apply[A](leftTop: Point[A], width: Width[A], height: Height[A])(implicit num: Numeric[A]): Rect[A] = {
    import num._
    require(gteq(width.n, one))
    require(gteq(height.n, one))

    Rect[A](leftTop, leftTop.move(minus(width.n, one), minus(height.n, one)))
  }
}

object RectDims {
  def unapply[A](rect: Rect[A]): Option[(A, A)] = {
    Some((rect.width, rect.height))
  }
}
