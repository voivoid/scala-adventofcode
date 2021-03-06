package adventOfCode.problems
package year2015

import java.security.MessageDigest

object problem04 extends baseProblem {

  override def solve1(input: Input): Int = {
    findPostfixes(getSecretKey(input), 5).next()
  }

  override def solve2(input: Input): Int = {
    findPostfixes(getSecretKey(input), 6).next()
  }

  def findPostfixes(secretKey: String, leadingZeroes: Int): Iterator[Int] = {
    import java.security.MessageDigest

    val digestInstance = MessageDigest.getInstance("MD5")

    Iterator
      .from(0)
      .filter(postfix => {
        hasLeadingZeroes(digestInstance, secretKey + postfix.toString, leadingZeroes)
      })
  }

  private def getSecretKey(input: Input) = input.mkString.trim

  private def hasLeadingZeroes(messageDigest: MessageDigest, str: String, leadingZeroes: Int): Boolean = {
    val hashBytes = messageDigest.digest(str.getBytes())

    def checkLeadingZeroes(zeroesLeft: Int, byteIndex: Int): Boolean = zeroesLeft match {
      case 0 => true
      case n if n == 1 => {
        val b = hashBytes(byteIndex)
        b >= 0 && b <= 15
      };
      case n => hashBytes(byteIndex) == 0 && checkLeadingZeroes(n - 2, byteIndex + 1)
    }

    checkLeadingZeroes(leadingZeroes, 0)
  }

}
