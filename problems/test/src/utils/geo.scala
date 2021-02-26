package adventOfCode.utils.tests

object geo extends utest.TestSuite {

  import utest._
  val tests = Tests {

    import adventOfCode.utils.geo.{Point, Rect}

    test("test rect properties") {
      val rect = Rect(Point(1, 3), Point(4, 7))
      rect.width ==> 4
      rect.height ==> 5
      rect.area ==> 20
    }

    test("test width and height with negative coords") {
      val rect = Rect(Point(-3, -3), Point(3, 3))
      rect.width ==> 7
      rect.height ==> 7
      rect.area ==> 49
    }

    test("test rect contains point") {
      val rect = Rect(Point(1, 1), Point(2, 2))
      assert(rect.contains(Point(1, 1)))
      assert(rect.contains(Point(1, 2)))
      assert(rect.contains(Point(2, 1)))
      assert(rect.contains(Point(2, 2)))

      assert(!rect.contains(Point(1, 0)))
      assert(!rect.contains(Point(0, 1)))
      assert(!rect.contains(Point(3, 2)))
      assert(!rect.contains(Point(2, 3)))
    }

    test("test rect contains other rect") {
      val rect1 = Rect(Point(1, 1), Point(4, 4))
      val rect2 = Rect(Point(2, 2), Point(3, 3))
      assert(rect1.contains(rect2))

      val rect3 = Rect(Point(6, 6), Point(7, 7))
      assert(!rect1.contains(rect3))
    }

    test("test rect contains itself") {
      val rect = Rect(Point(1, 1), Point(2, 2))
      assert(rect.contains(rect))
    }

    test("test rect intersects other rect") {
      val rect1 = Rect(Point(1, 1), Point(2, 2))
      val rect2 = Rect(Point(2, 0), Point(3, 3))
      assert(rect1.intersects(rect2))

      val rect3 = Rect(Point(5, 5), Point(6, 6))
      assert(!rect1.intersects(rect3))

      val rect4 = Rect(Point(3, 1), Point(6, 4))
      val rect5 = Rect(Point(0, 4), Point(3, 6))
      assert(rect4.intersects(rect5))
    }
  }

}
