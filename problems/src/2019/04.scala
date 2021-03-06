package adventOfCode.problems
package year2019

object problem04 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, hasAdjacentDigits)
  }

  override def solve2(input: Input): Int = {
    solve(input, hasNoMoreThanTwoAdjacentDigits)
  }

  private type Password = List[Char]
  private def PasswordDigits = 6

  import adventOfCode.utils.algorithms.IteratorSlidingTuple

  private def hasAdjacentDigits(password: Password): Boolean = {
    password.iterator.sliding2.exists { case (a, b) => a == b }
  }

  private def hasNoMoreThanTwoAdjacentDigits(password: Password): Boolean = {
    password match {
      case a :: b :: c :: rest if a == b && b == c => hasNoMoreThanTwoAdjacentDigits(rest.dropWhile(_ == c))
      case a :: b :: _ if a == b                   => true
      case _ :: rest                               => hasNoMoreThanTwoAdjacentDigits(rest)
      case Nil                                     => false
    }
  }

  private def solve(input: Input, checkAdjacentDigits: Password => Boolean): Int = {

    def isValidPassword(password: Password): Boolean = {
      checkAdjacentDigits(password) && password.iterator.sliding2.forall { case (a, b) => a <= b }
    }

    def nextValidPassword(password: Password): Password = {
      Iterator.iterate(nextIncreasingPassword(password))(nextIncreasingPassword).find(checkAdjacentDigits).get
    }

    def getPasswordRange(input: Input): (Password, Password) = {
      val (from, to) = parseRange(input.mkString)
      val validFrom = if (isValidPassword(from)) from else nextValidPassword(from)
      val validTo = if (isValidPassword(to)) to else nextValidPassword(to)

      (validFrom, validTo)
    }

    val (validFrom, validTo) = getPasswordRange(input)
    val validPasswords = Iterator.iterate(validFrom)(nextValidPassword)
    validPasswords.takeWhile(password => password != validTo).size
  }

  private def parseRange(input: String): (Password, Password) = {
    import fastparse._
    import SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, digit}

    def numStr[_: P] = P(digit).repX(1).!
    def rangeParser[_: P] = P(numStr ~ "-" ~ numStr)

    val (from, to) = parseValue(input, rangeParser(_))
    (from.toList, to.toList)
  }

  private def nextChar(c: Char): Char = {
    assert(c != '9')
    (c.toInt + 1).toChar
  }

  private def nextIncreasingPassword(password: Password): Password = {
    def impl(password: List[(Char, Int)]): Password = password match {
      case (a, index) :: (b, _) :: _ if b == '9' => List.fill(PasswordDigits - index)(nextChar(a))
      case (a, index) :: (b, _) :: _ if a > b    => a :: List.fill(PasswordDigits - index - 1)(a)
      case (a, _) :: Nil                         => nextChar(a) :: Nil
      case (a, _) :: rest                        => a :: impl(rest)
      case Nil                                   => sys.error("not expected")
    }

    impl(password.zipWithIndex)
  }

  private[problems] def implTests(): Unit = {
    import utest._

    nextIncreasingPassword("111111".toList) ==> "111112".toList
    nextIncreasingPassword("111119".toList) ==> "111122".toList
    nextIncreasingPassword("111199".toList) ==> "111222".toList
    nextIncreasingPassword("118999".toList) ==> "119999".toList
    nextIncreasingPassword("155333".toList) ==> "155555".toList
    nextIncreasingPassword("900000".toList) ==> "999999".toList
  }
}
