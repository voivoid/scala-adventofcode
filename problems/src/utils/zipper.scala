package adventOfCode.utils
package collections {

  object Zipper {
    def apply[A](a: A*): Zipper[A] = apply(a.toList)
    def apply[A](l: List[A]): Zipper[A] = new Zipper(Nil, l)
    def unapplySeq[A](z: Zipper[A]): Option[Seq[A]] = Some(z.right)
  }

  class Zipper[A] protected (private val left: List[A], private val right: List[A]) {
    require(!right.isEmpty)

    def next: Zipper[A] = if (hasNext) make(right.head :: left, right.tail) else sys.error("no more next elements")
    def next(n: Int): Zipper[A] = n match {
      case 0          => this
      case n if n < 0 => prev(n.abs)
      case n          => LazyList.iterate(this)(_.next)(n)
    }

    def hasNext = right.lengthCompare(2) >= 0

    def prev: Zipper[A] = if (hasPrev) make(left.tail, left.head :: right) else sys.error("no more prev elements")
    def prev(n: Int): Zipper[A] = n match {
      case 0          => this
      case n if n < 0 => next(n.abs)
      case n          => LazyList.iterate(this)(_.prev)(n)
    }

    def hasPrev = !left.isEmpty

    def current: A = right.head
    def resetCurrent: Zipper[A] = make(Nil, list)

    def update(a: A): Zipper[A] = make(left, a :: right.tail)
    def remove: Zipper[A] = {
      if (hasNext) make(left, right.tail)
      else {
        make(Nil, left.reverse)
      }
    }

    def isEmpty: Boolean = left.isEmpty && right.isEmpty

    def iterator: Iterator[A] = list.iterator
    def list: List[A] = left.foldLeft(right) { case (acc, e) => e :: acc }

    def map[B](f: A => B): Zipper[B] = make(left.map(f), right.map(f))

    override def hashCode(): Int = list.##
    override def equals(obj: Any): Boolean = obj match {
      case z: Zipper[A] => list == z.list
      case _            => false
    }

    protected def make[B](left: List[B], right: List[B]): Zipper[B] = new Zipper[B](left, right)
  }

  object CycledZipper {
    def apply[A](a: A*): CycledZipper[A] = apply(a.toList)
    def apply[A](l: List[A]): CycledZipper[A] = new CycledZipper(Nil, l)
  }

  class CycledZipper[A] protected (private val left: List[A], private val right: List[A]) extends Zipper[A](left, right) {
    override def next: Zipper[A] = if (hasNext) super.next else make(Nil, list)
    override def prev: Zipper[A] = if (hasPrev) super.prev
    else {
      val reversed = right.reverse
      make(reversed.tail, List(reversed.head))
    }
    protected override def make[B](left: List[B], right: List[B]): Zipper[B] = new CycledZipper[B](left, right)
  }

}
