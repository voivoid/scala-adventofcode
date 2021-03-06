package adventOfCode.problems
package year2016

object problem05 extends baseProblem {

  import year2015.problem04.findPostfixes

  override def solve1(input: Input): String = {
    val doorId = getDoorId(input)
    getLeadingZeroHashes(doorId).take(passwordChars).map(hash => hash(5)).mkString
  }

  override def solve2(input: Input): String = {
    val doorId = getDoorId(input)
    val leadingZeroHashes = getLeadingZeroHashes(doorId)

    val initPasswordState = Vector.fill[Option[Char]](passwordChars)(None)

    val passwordDecryptingStates = leadingZeroHashes.scanLeft(initPasswordState) {
      case (password, hash) => {
        val indexOpt = posCharToIndex(hash(5))
        val char = hash(6)

        if (indexOpt.isDefined && password(indexOpt.get).isEmpty) {
          password.updated(indexOpt.get, Some(char))
        } else password
      }
    }

    val decryptedPassword = passwordDecryptingStates.find(_.forall(_.isDefined)).get
    decryptedPassword.map(_.get).mkString
  }

  private def passwordChars = 8
  private def leadingZeroes = 5

  private def posCharToIndex(pos: Char): Option[Int] = {
    val digit = pos.asDigit
    if (digit >= 0 && digit < passwordChars) Some(digit) else None
  }

  private def hash(str: String): String = {
    import java.security.MessageDigest
    MessageDigest.getInstance("MD5").digest(str.getBytes).map("%02x".format(_)).mkString
  }

  private def getLeadingZeroHashes(doorId: String): Iterator[String] = {
    findPostfixes(doorId, leadingZeroes).map(postfix => hash(doorId + postfix.toString))
  }

  private def getDoorId(input: Input) = input.mkString.trim

}
