package adventOfCode.utils
package collections {

  object Zipper {
    def apply[A](a: A*): Zipper[A] = apply(a.toList)
    def apply[A](l: List[A]): Zipper[A] = new Zipper(Nil, l)
    def unapplySeq[A](z: Zipper[A]): Option[Seq[A]] = Some(z.right)
  }

  class Zipper[A](private val left: List[A], private val right: List[A]) {
    import algorithms.IteratorLast

    def isEmpty: Boolean = left.isEmpty && right.isEmpty

    def next: Zipper[A] = if (right.isEmpty) this else new Zipper[A](right.head :: left, right.tail)
    def next(n: Int): Zipper[A] = n match {
      case 0          => this
      case n if n < 0 => prev(n.abs)
      case n          => Iterator.iterate(this, n + 1)(_.next).last
    }

    def prev: Zipper[A] = if (left.isEmpty) this else new Zipper[A](left.tail, left.head :: right)
    def prev(n: Int): Zipper[A] = n match {
      case 0          => this
      case n if n < 0 => next(n.abs)
      case n          => Iterator.iterate(this, n + 1)(_.prev).last
    }

    def current: A = right.head
    def update(a: A): Zipper[A] = new Zipper[A](left, a :: right.tail)

    def remove: Zipper[A] = new Zipper[A](left, right.tail)

    def list: List[A] = left.foldRight(right)(_ :: _)
  }

}
