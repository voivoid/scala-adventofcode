package adventOfCode.utils

import scala.collection.{AbstractView, BuildFrom}
import scala.collection.generic.IsSeq
import scala.language.implicitConversions

package algorithms {
  class CollectionSlidingTuple[Repr, S <: IsSeq[Repr]](coll: Repr, seq: S) {
    private type B = (seq.A, seq.A)
    def sliding2[That](implicit bf: BuildFrom[Repr, B, That]): That = {
      val seqOps = seq(coll)
      bf.fromSpecific(coll)(new AbstractView[B] {
        def iterator = seqOps.iterator.sliding2
      })
    }
  }

}

package object algorithms {

  def powInt(n: Int, pow: Int): Int = {
    require(pow >= 0)
    if (pow == 0) 1
    else Math.multiplyExact(n, powInt(n, pow - 1))
  }

  implicit def CollectionSlidingTuple[Repr](coll: Repr)(implicit seq: IsSeq[Repr]): CollectionSlidingTuple[Repr, seq.type] =
    new CollectionSlidingTuple(coll, seq)

  implicit class IteratorLast[A](iterator: Iterator[A]) {
    def last: A = {
      assert(iterator.hasNext)
      iterator.foldLeft(iterator.next()) { case (_, v) => v }
    }
  }

  implicit class IteratorFindFirstDuplicate[A](iter: Iterator[A]) {
    def findFirstDuplicate: Option[A] = {
      if (!iter.hasNext) {
        None
      } else {
        val first = iter.next()
        iter
          .scanLeft((Set.empty[A], first)) { case ((set, prev), elem) =>
            (set + prev, elem)
          }
          .find { case (set, elem) =>
            set(elem)
          }
          .map { case (_, elem) => elem }
      }
    }
  }

  implicit class IteratorSlidingTuple[A](iter: Iterator[A]) {
    def sliding2: Iterator[(A, A)] = {
      if (!iter.hasNext) {
        Iterator.empty
      } else {
        new Iterator[(A, A)] {
          override def hasNext: Boolean = iter.hasNext

          override def next(): (A, A) = {
            val second = iter.next()
            val result = (first, second)
            first = second

            result
          }

          var first: A = iter.next()
        }
      }
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
