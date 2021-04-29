package adventOfCode.problems
package year2015

object problem20 extends baseProblem {

  override def solve1(input: Input): Int = {
    findFirstHouse(input, presentsMul = 10, maxStops = Int.MaxValue)
  }

  override def solve2(input: Input): Int = {
    findFirstHouse(input, presentsMul = 11, maxStops = 50)
  }

  private def findFirstHouse(input: Input, presentsMul: Int, maxStops: Int): Int = {
    val presentsToFind = input.mkString.toInt

    val highestHouseNum = presentsToFind / presentsMul
    val housesPerCheck = highestHouseNum / 4

    (1 to highestHouseNum)
      .grouped(housesPerCheck)
      .map(r => findFirstHouse(r.head, r.last, presentsToFind, presentsMul, maxStops))
      .collectFirst { case Some(i) =>
        i
      }
      .getOrElse(sys.error("no solution"))
  }

  private def findFirstHouse(houseFrom: Int, houseTo: Int, presents: Int, presentsMul: Int, maxStops: Int): Option[Int] = {
    val arr = Array.fill(houseTo - houseFrom + 1)(0)

    for {
      elfN <- 1 to houseTo
      houseId <- roundUp(houseFrom, elfN) * elfN to houseTo by elfN
      if (houseId / elfN) <= maxStops
    } arr(houseId - houseFrom) += elfN * presentsMul

    val resultHouseId = arr.indexWhere(_ >= presents)
    if (resultHouseId == -1) None else Some(resultHouseId + houseFrom)
  }

  private def roundUp(dividend: Int, divisor: Int): Int = {
    import scala.math.Integral.Implicits._
    val (quo, rem) = dividend /% divisor
    quo + (if (rem != 0) 1 else 0)
  }

}
