package adventOfCode.problems
package year2019

object problem16 extends baseProblem {

  override def solve1(input: Input): String = {
    solve1(input, 100)
  }

  private[problems] def solve1(input: Input, phases: Int): String = {
    val digits = input.mkString.map(_.asDigit).toVector
    val phaseIterations = phases + 1

    import adventOfCode.utils.algorithms.IteratorLast
    val finalDigits = Iterator.iterate(digits, phaseIterations)( runPhase ).last

    finalDigits.take(8).mkString
  }

  override def solve2(input: Input): String = {
    import scala.math.Integral.Implicits._
    val inputStr = input.mkString

    val inputDigits = inputStr.map(_.asDigit).toList
    val digitsTotal = inputDigits.size * 10000

    val digitsToSkip = inputStr.take(7).toInt
    assert(digitsToSkip * 2 > digitsTotal)

    val digitsToCalc = digitsTotal - digitsToSkip

    val (fullCycles, semiCycle) = digitsToCalc /% inputDigits.size

    val firstPhaseDigits = inputDigits.drop(inputDigits.size - semiCycle) ++
      List.fill(fullCycles)(inputDigits).flatten
    assert(firstPhaseDigits.size == digitsToCalc)

    runP2Phases(firstPhaseDigits, 100).take(8).mkString
  }

  @scala.annotation.tailrec
  private def runP2Phases(digits: List[Int], phasesLeft: Int): List[Int] = {
    if(phasesLeft == 0) digits
    else {
      val nextPhase = digits.foldRight(List.empty[Int]){
        case (e, Nil) => List(crop(e))
        case (e, acc@(x :: _)) => crop(e + x) :: acc
      }

      runP2Phases(nextPhase, phasesLeft - 1)
    }
  }

  private type Digits = Vector[Int]
  private def runPhase(digits: Digits): Digits = {
    digits.indices.map(dIndex => {
      val sum = digits.iterator.zipWithIndex.map{ case (d,i) => d * calcMul(dIndex, i) }.sum
      crop(sum)
    }).toVector
  }

  private def crop(n: Int): Int = n.abs % 10

  private def calcMul(dIndex: Int, i: Int): Int = {
    val n = ((i + 1) / (dIndex+1)) % 4

    if( n == 1 ) 1
    else if( n == 3 ) -1
    else 0
  }

}
