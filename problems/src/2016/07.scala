package adventOfCode.problems
package year2016

object problem07 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, checkTSLsupport)
  }

  override def solve2(input: Input): Int = {
    solve(input, checkSSLsupport)
  }

  private def solve(input: Input, checkAddr: (Nets, Nets) => Boolean): Int = {
    input
      .getLines()
      .count(addr => {
        val (supernets, hypernets) = splitNets(addr)
        checkAddr(supernets, hypernets)
      })
  }

  private type NetSeq = String
  private type Nets = List[NetSeq]

  private def checkTSLsupport(supernets: Nets, hypernets: Nets): Boolean = {
    supernets.exists(hasABBA) && !hypernets.exists(hasABBA)
  }

  private def checkSSLsupport(supernets: Nets, hypernets: Nets): Boolean = {
    val babSet = hypernets.flatMap(getABAs).toSet

    supernets
      .flatMap(getABAs)
      .exists(aba => {
        val bab = s"${aba(1)}${aba(0)}${aba(1)}"

        babSet.contains(bab)
      })
  }

  private def splitNets(addr: String): (Nets, Nets) = {
    val netsWithIndex = addr.split(Array('[', ']')).toList.zipWithIndex

    val (supernets, hypernets) = netsWithIndex.partition { case (_, index) => index % 2 == 0 }
    (supernets.map(_._1), hypernets.map(_._1))
  }

  private def hasABBA(netSeq: NetSeq): Boolean = {
    netSeq
      .sliding(4)
      .exists(s => {
        s(0) == s(3) && s(1) == s(2) && s(0) != s(1)
      })
  }

  private def getABAs(netSeq: NetSeq): Iterator[String] = {
    netSeq
      .sliding(3)
      .filter(s => {
        s(0) == s(2) && s(0) != s(1)
      })
  }

}
