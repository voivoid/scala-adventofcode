package adventOfCode.problems
package year2018

object problem08 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, calcMetadataSum)
  }

  override def solve2(input: Input): Int = {
    solve(input, calcRootNodeValue)
  }

  private def solve(input: Input, calcNode: Node => Int): Int = {
    import adventOfCode.utils.algorithms.IteratorSplit
    val (node, restIter) = parseNode(input.splitBy(' ').map(_.toInt))
    assert(!restIter.hasNext)

    calcNode(node)
  }

  private case class Node(children: Vector[Node], metadata: Seq[Int])

  private def calcMetadataSum(node: Node): Int = {
    node.metadata.sum + node.children.map(calcMetadataSum).sum
  }

  private def calcRootNodeValue(node: Node): Int = {
    if (node.children.isEmpty) {
      node.metadata.sum
    } else {
      val childNodeValues = node.metadata.iterator
        .map(index => index - 1)
        .filter(index => node.children.indices.contains(index))
        .map(index => calcRootNodeValue(node.children(index)))

      childNodeValues.sum
    }
  }

  private def parseNode(numsIter: Iterator[Int]): (Node, Iterator[Int]) = {
    val childNodesNum = numsIter.next()
    val metadataEntriesNum = numsIter.next()

    val (childNodes, pastChildNodesIter) = LazyList.iterate((Vector.empty[Node], numsIter)) {
      case (nodesAcc, iter) => {
        val (node, iter2) = parseNode(iter)
        (nodesAcc.appended(node), iter2)
      }
    }(childNodesNum)

    val (metadataEntries, restIter) = pastChildNodesIter.splitAt(metadataEntriesNum)

    (Node(childNodes, metadataEntries.toSeq), restIter)
  }

}
