package adventOfCode.problems
package year2020

object problem02 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, isValid1)
  }

  override def solve2(input: Input): Int = {
    solve(input, isValid2)
  }

  private def solve(input: Input, isValid: Entry => Boolean): Int = {
    input.getLines().map(parseEntry).count(isValid)
  }

  private def isValid1(entry: Entry): Boolean = {
    val Entry(least, most, letter, password) = entry
    val lettersCount = password.count(_ == letter)

    lettersCount >= least && lettersCount <= most
  }

  private def isValid2(entry: Entry): Boolean = {
    val Entry(i1, i2, letter, password) = entry

    password.size >= i2 &&
    (password(i1 - 1) == letter ^ password(i2 - 1) == letter)
  }

  private[problems] case class Entry(least: Int, most: Int, letter: Char, password: String)

  private[problems] def parseEntry(entryStr: String): Entry = {
    import fastparse._
    import SingleLineWhitespace._
    import adventOfCode.utils.parse.{num, alpha, parseValue}

    def occurances[_: P] = num ~ "-" ~ num

    def password[_: P] = P(CharIn("a-zA-Z")).rep(1).!

    def parser[_: P] = (occurances ~ alpha ~ ":" ~ password).map(Entry tupled _)

    parseValue(entryStr, parser(_))
  }

}
