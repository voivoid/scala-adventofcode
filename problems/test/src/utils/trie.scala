package adventOfCode.utils.tests

object trie extends utest.TestSuite {

  import utest._
  import adventOfCode.utils.collections.CharTrie

  val tests = Tests {

    test("empty trie have no nodes") {
      assert(CharTrie().getNodes.isEmpty)
    }

    test("empty trie contains nothing") {
      assert(!CharTrie().contains("a"))
    }

    test("the same tree is returned if an empty string is inserted") {
      val trie = CharTrie()
      val trie2 = trie.insert("")

      assert(trie.eq(trie2))
    }

    test("inserted string and it's substrings should be found") {
      val trie = CharTrie().insert("hello")

      assert(trie.contains("hello"))
      assert(trie.contains("hell"))
      assert(!trie.contains("world"))
    }

    test("multiple consecutive inserts starting with different letters") {
      val trie = CharTrie().insert("ab").insert("cd").insert("ef")

      trie.getNodes.size ==> 3
      assert(trie.node('a').map(_.node('b')).isDefined)
      assert(trie.node('c').map(_.node('d')).isDefined)
      assert(trie.node('e').map(_.node('f')).isDefined)
    }

    test("multiple consecutive inserts starting with same letter") {
      val trie = CharTrie().insert("a").insert("ab").insert("abc")

      trie.getNodes.size ==> 1
      assert(trie.node('a').map(_.node('b').map(_.node('c'))).isDefined)
    }

    test("insertion of an existing string should have no effect") {
      val trie = CharTrie().insert("abc")
      val trie2 = trie.insert("a").insert("ab").insert("abc")

      assert(trie.eq(trie2))
    }

  }
}
