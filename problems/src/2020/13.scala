package adventOfCode.problems
package year2020

object problem13 extends baseProblem {

  override def solve1(input: Input): Int = {
    val lines = input.getLines()
    val earliestTimeStamp = lines.next().toInt
    val busIds = lines.next().split(',').filter( _ != "x").map(_.toInt)

    val( id, minToWait ) = busIds.map( id => (id, id - earliestTimeStamp % id ) ).minBy(_._2)

    id * minToWait
  }

  override def solve2(input: Input): Long = {
    val lines = input.getLines()
    lines.next() // skip lines

    val busIds = lines.next().split(',').zipWithIndex.filter{ case (id, _) => id != "x" }.map{ case (id, index) => (id.toLong, index)}

    val r = busIds.foldLeft((0L,1L)) {
      case ((step, mul), (busId, busIdx)) =>
        val i = Iterator.from(0).find(i => ((step + mul * i) + busIdx) % busId == 0 ).get
        (step + i.toLong * mul, mul * busId)
    }

    r._1
  }



}
