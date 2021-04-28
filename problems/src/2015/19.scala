package adventOfCode.problems
package year2015

object problem19 extends baseProblem {

  import scala.collection.immutable.MultiDict

  override def solve1(input: Input): Int = {
    val (reactions, molecule) = parseInput(input)

    val moleculeReplacements = elementSubstrings(molecule).flatMap {
      case Substring(from, len) => {
        val replacements = reactions.get(molecule.substring(from, from + len))

        replacements.map(r => molecule.patch(from, r, len))
      }
    }.toSet

    moleculeReplacements.size
  }

  override def solve2(input: Input): Int = {
    val (_, molecule) = parseInput(input)

    val rn = molecule.sliding(2).count(_ == "Rn")
    val ar = molecule.sliding(2).count(_ == "Ar")
    val y = molecule.count(_ == 'Y')

    val molecules = molecule.count(_.isUpper)
    if( rn + ar + y == 0) molecules else molecules - rn - ar - 2 * y - 1
  }

  private case class Substring(from: Int, len: Int)
  private def elementSubstrings(molecule: String): List[Substring] = {
    List.unfold(0) {
      case idx if idx == molecule.size => None
      case idx => {
        val len = {
          val isTwoLetter = idx + 1 < molecule.size && molecule(idx + 1).isLower
          if (isTwoLetter) 2 else 1
        }

        Some((Substring(idx, len), idx + len))
      }
    }
  }

  private def parseInput(input: Input): (MultiDict[String, String], String) = {
    val lines = input.getLines().toVector

    val molecule = lines.last
    val reactions = lines
      .dropRight(2)
      .map(s => {
        val Array(left, right) = s.split(" => ")
        left -> right
      })
      .to(MultiDict)

    (reactions, molecule)
  }

}
