package adventOfCode.problems
package year2018

object problem05 extends baseProblem {

  import adventOfCode.utils.collections.Zipper

  override def solve1(input: Input): Int = {
    val polymer = Zipper(input.toList)
    calcReducedPolymerSize(polymer)
  }

  override def solve2(input: Input): Int = {
    val polymerUnits = input.toList

    val lowercaseUnitsSet = polymerUnits.iterator.map(_.toLower).toSet

    def makeFilteredPolymer(unitToFilter: Char): PolymerZipper = {
      Zipper(polymerUnits.filter(_.toLower != unitToFilter))
    }

    lowercaseUnitsSet.iterator.map(unit => calcReducedPolymerSize(makeFilteredPolymer(unit))).min
  }

  private type PolymerZipper = Zipper[Char]

  private def calcReducedPolymerSize(polymer: PolymerZipper): Int = reducePolymer(polymer).list.size

  private def hasOppositePolarity(c1: Char, c2: Char): Boolean = {
    val diff = 'a' - 'A'
    (c1 - c2).abs == diff
  }

  @scala.annotation.tailrec
  private def reducePolymer(polymer: PolymerZipper): PolymerZipper = polymer match {
    case Zipper(a, b, _*) if hasOppositePolarity(a, b) => {
      val reduced = polymer.removeCurrent.removeCurrent
      val prevReduced = if (reduced.hasPrev) reduced.prev else reduced
      reducePolymer(prevReduced)
    }
    case Zipper(_, _, _*) => reducePolymer(polymer.next)
    case _                => polymer
  }

}
