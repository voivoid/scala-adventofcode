package adventOfCode.utils.algorithms

package object toposort {

  def topologicalSortLexiOrdered[A: Ordering](nodes: Iterable[topoNode[A]]): List[A] = {
    import adventOfCode.utils.algorithms.IteratorGroupValuesByTheirLength
    import scala.collection.immutable.TreeSet

    val childrenMap = nodes.groupMap(_.parent)(_.child)
    val parentCounterMap = {
      nodes.iterator.map(node => (node.parent, 0)).toMap ++
        nodes.iterator.map(_.child).groupValuesByTheirLength
    }

    val orderedNodesWithZeroParents =
      parentCounterMap.iterator.collect { case (node, 0) => node }.to(TreeSet)

    List.unfold((orderedNodesWithZeroParents, parentCounterMap)) {
      case (noParentsNodes, parentCounterMap) if noParentsNodes.isEmpty => {
        assert(parentCounterMap.values.forall(_ == 0)) // otherwise the graph has cycles
        None
      }

      case (noParentsNodes, parentCounterMap) => {
        val resultNode = noParentsNodes.head
        val resultNodeChildren = childrenMap.getOrElse(resultNode, Seq.empty)

        val updatedState = resultNodeChildren.foldLeft((noParentsNodes.tail, parentCounterMap)) {
          case ((noParentsNodesAcc, parentCounterMapAcc), childNode) => {
            val decrementedParentCounterMap = parentCounterMapAcc.updatedWith(childNode)(counter => counter.map(_ - 1))
            val updatedNoParentsNodes =
              if (decrementedParentCounterMap(childNode) == 0) noParentsNodesAcc + childNode else noParentsNodesAcc

            (updatedNoParentsNodes, decrementedParentCounterMap)
          }
        }

        Some((resultNode, updatedState))
      }
    }
  }

}

package toposort {

  trait topoNode[A] {
    val parent: A
    val child: A
  }

}
