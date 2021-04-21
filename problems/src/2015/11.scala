package adventOfCode.problems
package year2015

object problem11 extends baseProblem {

  override def solve1(input: Input): String = {
    nextSecurePassword(input.toList).mkString
  }

  override def solve2(input: Input): String = {
    val p1 = nextSecurePassword(input.toList)
    val p2 = nextSecurePassword(p1)
    p2.mkString
  }

  private type Password = List[Char]

  private def nextSecurePassword(password: Password): Password = {
    val reversed = nextPasswordWithoutForbiddenLetters(password).reverse

    val nextSecureReversedPass = if (isSimpleCase(reversed)) {
      nextSimplePassword(reversed)
    } else {
      Iterator.iterate(incPassword(reversed))(incPassword).find(isGoodPassword).get
    }

    nextSecureReversedPass.reverse
  }

  private def nextPasswordWithoutForbiddenLetters(password: Password): Password = password match {
    case Nil                             => Nil
    case c :: cs if isForbiddenLetter(c) => nextChar(c) :: List.fill(cs.size)('a')
    case c :: cs                         => c :: nextPasswordWithoutForbiddenLetters(cs)
  }

  private def nextChar(c: Char): Char = c match {
    case 'h' => 'j'
    case 'n' => 'p'
    case 'k' => 'm'
    case 'z' => 'a'
    case c   => (c + 1).toChar
  }

  private def incPassword(reversedPassword: Password): Password = reversedPassword match {
    case Nil       => List('a')
    case 'z' :: cs => 'a' :: incPassword(cs)
    case c :: cs   => nextChar(c) :: cs
  }

  private def isForbiddenLetter(c: Char): Boolean = c == 'i' || c == 'o' || c == 'l'

  private def isGoodPassword(reversedPassword: Password): Boolean = {
    hasIncreasingThreeLetters(reversedPassword) && hasNonOverlappingLettersPair(reversedPassword)
  }

  private def hasIncreasingThreeLetters(reversedPassword: Password): Boolean = {
    reversedPassword.sliding(3).exists {
      case List(a, b, c) => a - 1 == b && a - 2 == c
      case _             => false
    }
  }

  private def hasNonOverlappingLettersPair(reversedPassword: Password): Boolean = {
    import adventOfCode.utils.algorithms.IteratorSlidingTuple
    reversedPassword.iterator.sliding2.filter { case (a, b) => a == b }.toSet.size >= 2
  }

  // minimal length secure password has a form of AABCC where A, B, C
  // are some consecutive letters like 'ccdee' or 'xxyzz'
  // ( since AA and CC are two non-overlapping pairs and ABC are increasing three letters ).
  //
  // if a password length is <= 5 chars it can be replaced with AABCC
  // if none of requirements are fulfilled OR a password already ends with AABCC postfix
  // then a next password can be calculated by replacing last 5 chars with an AABCC postfix
  private def isSimpleCase(reversedPassword: Password): Boolean = {
    reversedPassword.size <= 5 || (!hasIncreasingThreeLetters(reversedPassword) && !hasNonOverlappingLettersPair(reversedPassword)) || {
      val last5 = reversedPassword.take(5)
      last5.size < 5 || last5 == reversedPostfixStartingFrom(last5.last)
    }
  }

  private def nextSimplePassword(reversedPassword: Password): Password = {
    val (postfix, rest) = reversedPassword.splitAt(5)
    if (postfix.size == 5) {
      val postfixBaseChar = postfix.last

      val newPostfixBaseChar =
        if (less(postfix, simpleReversedPostfix(postfixBaseChar)._1) || postfixBaseChar == 'z')
          postfixBaseChar
        else
          nextChar(postfixBaseChar)

      val (newPostfix, incRest) = simpleReversedPostfix(newPostfixBaseChar)
      newPostfix ::: (if (incRest) incPassword(rest) else rest)
    } else {
      reversedPostfixStartingFrom('a')
    }
  }

  private def simpleReversedPostfix(c: Char): (Password, Boolean) = {
    if (c == 'y' || c == 'z') (reversedPostfixStartingFrom('a'), true)
    else if (c >= 'g' && c <= 'o') (reversedPostfixStartingFrom('p'), false)
    else (reversedPostfixStartingFrom(c), false)
  }

  private def reversedPostfixStartingFrom(a: Char): Password = {
    val b = nextChar(a)
    val c = nextChar(b)
    List(c, c, b, a, a)
  }

  private def less(postfix1: Password, postfix2: Password): Boolean = {
    val firstMismatch = postfix1.reverseIterator.zip(postfix2.reverseIterator).find { case (c1, c2) => c1 != c2 }
    firstMismatch.exists { case (c1, c2) => c2 > c1 }
  }
}
