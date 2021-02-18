package adventOfCode.utils
package object algorithms {

  implicit class SlidingTuple[A](iterator: scala.collection.Iterator[A]) {
    def sliding2: Iterator[(A, A)] = {
      val (i1, i2) = iterator.duplicate
      i1.zip(i2.drop(1))
    }
  }

  implicit class IteratorMinMax[T: Numeric](iterator: scala.collection.Iterator[T]) {
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

  implicit class IterableMinMax[T: Numeric](coll: scala.collection.Iterable[T]) {
    def minmax: (T, T) = {
      coll.iterator.minmax
    }
  }

}
