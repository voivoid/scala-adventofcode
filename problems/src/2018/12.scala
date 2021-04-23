package adventOfCode.problems
package year2018

import scala.collection.immutable.TreeSet

object problem12 extends baseProblem {

  override def solve1(input: Input): Long = {
    solve(input, 20L)
  }

  override def solve2(input: Input): Long = {
    solve(input, 50000000000L)
  }

  private def solve(input: Input, gensNum: Long): Long = {
    import adventOfCode.utils.algorithms.{IteratorSlidingTuple, IteratorLast}

    val lines = input.getLines()

    val initialGeneration = parseInitialGeneration(lines.next())
    lines.next() // skip empty line

    val rulesMap = lines.map(parseRule).map { case Rule(pattern, pot) => pattern -> pot }.toMap
    val generations = Iterator.iterate(initialGeneration)(calcNextGeneration(_, rulesMap))

    // it's worth to assume that generations will start to cycle before Int.MaxValue will be reached
    val gensNumInt = gensNum.min(Int.MaxValue).toInt

    val (lastUniqueGen, lastUniqueGenIdx) = {
      val (lastGen, lastIdx) = generations.sliding2.take(gensNumInt).takeWhile { case (s1, s2) => !isCycled(s1, s2) }.zipWithIndex.last
      (lastGen._2, (lastIdx + 1).toLong)
    }

    val lastUniquePotsSum = lastUniqueGen.foldLeft(0L)(_ + _)
    val cycledGens = (gensNum - lastUniqueGenIdx)
    lastUniquePotsSum + (cycledGens * lastUniqueGen.size)
  }

  private def isCycled(s1: Generation, s2: Generation): Boolean = {
    // this is just a heuristic; not 100% guaranteed to be correct
    s1.size == s2.size && (s2.sum - s1.sum) == s1.size
  }

  private type Pot = Char
  private case class Rule(pattern: String, pot: Pot)

  private type RulesMap = Map[String, Pot]
  private type Generation = TreeSet[Int]

  private def parseInitialGeneration(s: String): Generation = {
    val Array(_, genStr) = s.split(": ")

    genStr.iterator.zipWithIndex.filter { case (pot, _) => pot == '#' }.map(_._2).to(TreeSet)
  }

  private def parseRule(s: String): Rule = {
    val Array(ruleStr, potStr) = s.split(" => ")
    assert(potStr.size == 1)

    Rule(ruleStr, potStr.head)
  }

  private def calcNextGeneration(generation: Generation, rulesMap: RulesMap): Generation = {
    val first = generation.head - 3
    val last = generation.last + 3

    (first to last)
      .sliding(5)
      .zipWithIndex
      .map {
        case (slideRng, idx) => {
          val slideStr = slideRng.iterator.map(generation).map(if (_) '#' else '.').mkString
          (rulesMap.getOrElse(slideStr, '.'), idx + first + 2)
        }
      }
      .filter { case (p, _) => p == '#' }
      .map(_._2)
      .to(TreeSet)
  }

}
