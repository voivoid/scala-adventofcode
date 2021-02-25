package adventOfCode.utils
package collections {

  object CharTrie {
    def apply(): CharTrie = apply(RootValue)

    private def apply(c: Char): CharTrie = new CharTrie(Vector.fill(NodesNum)(null), c)

    private def NodesNum = 'z' - 'a' + 1

    private def RootValue = '\u0000'

    private def charIndex(c: Char) = {
      assert(c >= 'a' && c <= 'z')
      c - 'a'
    }
  }

  class CharTrie(private val nodes: Vector[CharTrie], val value: Char) {
    assert(nodes.size == CharTrie.NodesNum)

    def insert(s: String): CharTrie = {
      if (s.isEmpty) this
      else {
        val newNodes = s.iterator.scanLeft(this) { case (trie, x) =>
          trie.node(x).getOrElse(CharTrie(x))
        }

        newNodes.reduceRight[CharTrie] {
          case (trie, node) => {
            val idx = CharTrie.charIndex(node.value)
            val isChanged = trie.nodes(idx).ne(node)

            if (isChanged) new CharTrie(trie.nodes.updated(idx, node), trie.value)
            else trie
          }
        }
      }
    }

    def contains(s: String): Boolean =
      s.foldLeft[Option[CharTrie]](Some(this)) { case (trie, c) =>
        trie.flatMap(_.node(c))
      }.isDefined

    def getNodes: Vector[CharTrie] = nodes.filter(_ != null)

    def node(c: Char): Option[CharTrie] = {
      Option(nodes(CharTrie.charIndex(c)))
    }
  }

}
