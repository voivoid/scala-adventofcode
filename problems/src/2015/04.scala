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
    findPostfixesChunkByChunk(secretKey, leadingZeroes, 0, 100000)
  }

  private val cores = Runtime.getRuntime().availableProcessors();

  private def findPostfixesJob(secretKey: String, leadingZeroes: Int, range: Range): Iterator[Int] = {
    import java.security.MessageDigest
    val digestInstance = MessageDigest.getInstance("MD5")

    range
      .filter(postfix => {
        hasLeadingZeroes(digestInstance, secretKey + postfix.toString, leadingZeroes)
      })
      .iterator
  }

  private def runFindPostfixesJobs(secretKey: String, leadingZeroes: Int, range: Range): Iterator[Int] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration
    import scala.concurrent.{Await, Future}

    val ranges = breakRange(range, cores)
    val futures = ranges.map(range =>
      Future {
        findPostfixesJob(secretKey, leadingZeroes, range)
      }
    )

    val aggregatedFuture = Future.reduceLeft(futures)(_ ++ _)

    Await.result(aggregatedFuture, Duration.Inf)
  }

  private def findPostfixesChunkByChunk(secretKey: String, leadingZeroes: Int, from: Int, chunk: Int): Iterator[Int] = {
    val to = from + chunk
    val resultIterator = runFindPostfixesJobs(secretKey, leadingZeroes, from until to)
    resultIterator.concat(findPostfixesChunkByChunk(secretKey, leadingZeroes, to, chunk))
  }

  private def breakRange(totalRange: Range, totalCores: Int): List[Range] = {
    List
      .unfold((totalRange, totalCores)) {
        case (range, cores) if range.isEmpty || cores <= 0 => None
        case (range, cores) => {
          val itemsPerSubrange = range.size / cores
          val incSubrange = if (range.size % cores > 0) 1 else 0
          val (subRange, restRange) = range.splitAt(itemsPerSubrange + incSubrange)

          Some((subRange, (restRange, cores - 1)))
        }
      }
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
