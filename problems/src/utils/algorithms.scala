package adventOfCode.utils
package object algorithms {

  implicit class SlidingTuple[A](coll: scala.collection.Iterator[A]) {
    def sliding2: Iterator[(A, A)] = {
      val (i1, i2) = coll.iterator.duplicate
      i1.zip(i2.drop(1))
    }
  }

}
