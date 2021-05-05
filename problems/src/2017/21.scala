package adventOfCode.problems
package year2017

object problem21 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, 5)
  }

  override def solve2(input: Input): Int = {
    solve(input, 18)
  }

  def solve(input: Input, @scala.annotation.unused iterations: Int): Int = {
    val (rules2to3, rules3to4) = parseInput(input)
    val initG3 = ('.', '#', '.', '.', '.', '#', '#', '#', '#')

    def calcPixels(iterationsLeft: Int, i0: G3): Int = {
      lazy val i1 = rules3to4(i0)
      lazy val i2 = i1.map(rules2to3)
      lazy val i3 = G3sToG2s(i2).map(rules2to3)

      if (iterationsLeft == 0) sumProd(i0)
      else if (iterationsLeft == 1) sumProds(i1)
      else if (iterationsLeft == 2) sumProds(i2)
      else if (iterationsLeft == 3) sumProds(i3)
      else {
        i3.foldLeft((0, Map.empty[G3, Int])) {
          case ((accSum, accMap), i) => {
            if (accMap.contains(i)) {
              (accSum + accMap(i), accMap)
            } else {
              val s = calcPixels(iterationsLeft - 3, i)
              (accSum + s, accMap.updated(i, s))
            }
          }
        }._1
      }
    }

    calcPixels(iterations, initG3)
  }

  private def sumProd(p: Product): Int = {
    p.productIterator.count(_ == PixelOn)
  }

  private def sumProds(ps: List[Product]): Int = {
    ps.map(sumProd).sum
  }

  private def G3sToG2s(l: List[G3]): List[G2] = {
    val List(t1, t2, t3, t4) = l: @unchecked

    val (p01, p02, p03, p04, p05, p06, p07, p08, p09) = t1
    val (p10, p11, p12, p13, p14, p15, p16, p17, p18) = t2
    val (p19, p20, p21, p22, p23, p24, p25, p26, p27) = t3
    val (p28, p29, p30, p31, p32, p33, p34, p35, p36) = t4

    List(
      (p01, p02, p04, p05),
      (p03, p10, p06, p13),
      (p11, p12, p14, p15),
      (p07, p08, p19, p20),
      (p09, p16, p21, p28),
      (p17, p18, p29, p30),
      (p22, p23, p25, p26),
      (p24, p31, p27, p34),
      (p32, p33, p35, p36)
    )
  }

  private type G2 = (Char, Char, Char, Char)
  private type G3 = (Char, Char, Char, Char, Char, Char, Char, Char, Char)
  private type G4 = List[G2]

  private sealed trait Rule
  private case class Rule2(from: G2, to: G3) extends Rule
  private case class Rule3(from: G3, to: G4) extends Rule

  private def parseInput(input: Input): (Map[G2, G3], Map[G3, G4]) = {
    val rules = input.getLines().map(parseRule).toList

    val (rules2, rules3) = rules.partitionMap {
      case r: Rule2 => Left(r)
      case r: Rule3 => Right(r)
    }

    val m2Pairs = for {
      Rule2(from, to) <- rules2
      r <- rotations(from)
    } yield r -> to

    val m3Pairs = for {
      Rule3(from, to) <- rules3
      r <- rotations(from)
      f <- List(r, hFlip(r))
    } yield f -> to

    (m2Pairs.toMap, m3Pairs.toMap)
  }

  private def rotations(g2: G2): Set[G2] = {
    val r1 = g2
    val r2 = rotateClockwise(r1)
    val r3 = rotateClockwise(r2)
    val r4 = rotateClockwise(r3)

    Set(r1, r2, r3, r4)
  }

  private def rotations(g3: G3): Set[G3] = {
    val r1 = g3
    val r2 = rotateClockwise(r1)
    val r3 = rotateClockwise(r2)
    val r4 = rotateClockwise(r3)

    Set(r1, r2, r3, r4)
  }

  private def hFlip(g3: G3): G3 = {
    val (p1, p2, p3, p4, p5, p6, p7, p8, p9) = g3
    (p3, p2, p1, p6, p5, p4, p9, p8, p7)
  }

  private def rotateClockwise(g2: G2): G2 = {
    val (p1, p2, p3, p4) = g2
    (p3, p1, p4, p2)
  }

  private def rotateClockwise(g3: G3): G3 = {
    val (p1, p2, p3, p4, p5, p6, p7, p8, p9) = g3
    (p7, p4, p1, p8, p5, p2, p9, p6, p3)
  }

  private def parseRule(s: String): Rule = {
    val Array(from, to) = s.split(" => ")

    if (from.size == 5) Rule2(makeG2(from), makeG3(to))
    else Rule3(makeG3(from), makeG4(to))
  }

  private def makeG2(s: String): G2 = {
    (s(0), s(1), s(3), s(4))
  }

  private def makeG3(s: String): G3 = {
    (s(0), s(1), s(2), s(4), s(5), s(6), s(8), s(9), s(10))
  }

  private def makeG4(s: String): G4 = {
    val s1 = (s(0), s(1), s(5), s(6))
    val s2 = (s(2), s(3), s(7), s(8))
    val s3 = (s(10), s(11), s(15), s(16))
    val s4 = (s(12), s(13), s(17), s(18))

    List(s1, s2, s3, s4)
  }

  private val PixelOn = '#'

}
