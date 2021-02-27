package adventOfCode.utils
package object algorithms {

  implicit class IteratorLast[A](iterator: Iterator[A]) {
    def last: A = {
      assert(iterator.hasNext)
      iterator.foldLeft(iterator.next()) { case (_, v) => v }
    }
  }

  implicit class IteratorSlidingTuple[A](iterator: Iterator[A]) {
    def sliding2: Iterator[(A, A)] = {
      val (i1, i2) = iterator.duplicate // TODO: get rid of duplicate?
      i1.zip(i2.drop(1))
    }
  }

  implicit class IteratorMinMax[T: Numeric](iterator: Iterator[T]) {
    def minmax: (T, T) = {
      require(iterator.hasNext)
      val numeric = implicitly[Numeric[T]]
      val n = iterator.next()

      iterator.foldLeft((n, n)) {
        case ((min, max), v) => {
          (numeric.min(min, v), numeric.max(max, v))
        }
      }
    }
  }

  implicit class IterableMinMax[T: Numeric](coll: Iterable[T]) {
    def minmax: (T, T) = {
      coll.iterator.minmax
    }
  }

  implicit class IterableCycle[T](coll: Iterable[T]) {
    def cycled: Iterator[T] = {
      Iterator.continually(coll).flatten
    }
  }

  implicit class Tuple3Sorted[T: Numeric](t: Tuple3[T, T, T]) {
    def sorted: (T, T, T) = {
      val numeric = implicitly[Numeric[T]]

      val min = numeric.min[T] _
      val max = numeric.max[T] _
      val plus = numeric.plus _
      val minus = numeric.minus _

      val (a, b, c) = t

      val minV = min(min(a, b), c)
      val maxV = max(max(a, b), c)

      val abc = plus(plus(a, b), c)
      val minmax = plus(minV, maxV)

      val midV = minus(abc, minmax)

      (minV, midV, maxV)
    }
  }

  implicit class IteratorSplit(charIter: Iterator[Char]) {
    def splitBy(splitChar: Char): Iterator[String] = {
      makeSplitIterator(splitChar, charIter.buffered)
    }

    def makeSplitIterator(splitChar: Char, buffIter: scala.collection.BufferedIterator[Char]) =
      new scala.collection.AbstractIterator[String] {
        dropSplitChars()

        override def hasNext: Boolean = buffIter.hasNext
        override def next(): String = {
          val sb = new StringBuilder

          while (buffIter.hasNext && buffIter.head != splitChar) {
            sb.append(buffIter.next())
          }

          dropSplitChars()
          sb.result()
        }

        private def dropSplitChars() = {
          while (buffIter.hasNext && buffIter.head == splitChar) {
            buffIter.next()
          }
        }
      }
  }

}
