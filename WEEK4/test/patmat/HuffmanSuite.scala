package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times function works - char, number occurs") {
    new TestTrees {
      assert(times(List[Char]('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton function works") {
    new TestTrees {
      assert(singleton(List(t1)) === true) //contains only one single code tree expected true
      assert(singleton(List(t1, t2)) === false) //expected false
    }
  }

  test("combine of some leaf list works") {
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leafList) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until singleton function works") {
    new TestTrees {
      val t3 = List(t1, t2)
      val t4 = until(singleton, combine)(t3)
      assert(singleton(t4) === true)
    }
  }

  test("create code tree works") {
    val chars = "oops".toList
    val codeTree = createCodeTree(chars)
    val tst = Fork(Fork(Leaf('p', 1), Leaf('s', 1), List('p', 's'), 2), Leaf('o', 2), List('p', 's', 'o'), 4)
    assert(codeTree === tst)
  }

  test("encode function works") {
    new TestTrees {
      assert(encode(t1, string2Chars("abd")) === List(0, 1, 1))
      assert(encode(t1, string2Chars("bcd")) === List(1, 1, 1))
      assert(encode(t1, string2Chars("efgheff")) === List(1, 1, 1, 1, 1, 1, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1, "ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2, "abbda".toList)) === "abbda".toList)
    }
  }

  test("decode and QUICK encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, quickEncode(t2)("abbda".toList)) === "abbda".toList)
    }
  }

  test("code table bits for char") {
    val tab = List(('a', List(1)), ('b', List(0)), ('h', List(1, 0)), ('n', List(0, 1)))
    assert(codeBits(tab, 'h') === List(1, 0))
  }

  test("convert tree to table") {
    val tree = Fork(
      Fork(Leaf('p', 1), Leaf('s', 1), List('p', 's'), 2),
      Leaf('o', 2), List('p', 's', 'o'), 4)
    assert(convert(tree) === List(('p', List(0, 0)), ('s', List(0, 1)), ('o', List(1))))
  }

  test("quick encode function works") {
    new TestTrees {
      assert(quickEncode(t1)(string2Chars("abd")) === List(0, 1))
    }
  }

  test("decodedSecret function works") {
    new TestTrees {
      assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
    }
  }
}
