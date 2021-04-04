package adventOfCode.problems
package year2020

object problem06 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, countAnyYes)
  }

  override def solve2(input: Input): Int = {
    solve(input, countAllYes)
  }

  private def solve(input: Input, countYes: Iterator[Answers] => Int): Int = {
    val groups = parseGroups(input.mkString)
    groups.iterator
      .map(group => {
        val groupAnswers = group.iterator.map(_.toSet)
        countYes(groupAnswers)
      })
      .sum
  }

  private type Group = Seq[String]
  private type Answers = Set[Char]

  private def countAnyYes(groupAnswers: Iterator[Answers]): Int = {
    groupAnswers.reduceLeft(_.union(_)).size
  }

  private def countAllYes(groupAnswers: Iterator[Answers]): Int = {
    groupAnswers.reduceLeft(_.intersect(_)).size
  }

  private[problems] def parseGroups(input: String): Seq[Group] = {
    import fastparse._
    import NoWhitespace._
    import adventOfCode.utils.parse.{parseValue}

    def group[_: P] = P(CharIn("a-z").rep(1)).!.rep(sep = "\n")
    def groups[_: P] = group.rep(sep = "\n\n")

    parseValue(input, groups(_))
  }

}
