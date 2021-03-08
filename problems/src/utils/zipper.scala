package adventOfCode.utils
package collections {

  object Zipper {
    def apply[A](l: List[A]) = new Zipper(Nil, l)
  }

  class Zipper[A](left: List[A], right: List[A]) {
    import algorithms.IteratorLast

    def isEmpty: Boolean = left.isEmpty && right.isEmpty

    def next: Zipper[A] = new Zipper[A](right.head :: left, right.tail)
    def next(n: Int): Zipper[A] = n match {
      case 0          => this
      case n if n < 0 => prev(n.abs)
      case n          => Iterator.iterate(this, n + 1)(_.next).last
    }

    def prev: Zipper[A] = new Zipper[A](left.tail, left.head :: right)
    def prev(n: Int): Zipper[A] = n match {
      case 0          => this
      case n if n < 0 => next(n.abs)
      case n          => Iterator.iterate(this, n + 1)(_.prev).last
    }

    def current: A = right.head
    def update(a: A): Zipper[A] = new Zipper[A](left, a :: right.tail)
  }

}
