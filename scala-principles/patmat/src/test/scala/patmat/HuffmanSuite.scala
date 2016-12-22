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
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    val timez: List[(Char, Bit)] = times(string2Chars("ala"))
    assert(timez.contains(('l', 1)))
    assert(timez.contains(('a', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton list") {
    assert(singleton(List(Leaf('e', 1))))
    assert(!singleton(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))))
    assert(!singleton(List()))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("create code tree") {
    val x = createCodeTree(string2Chars("helle"))
    assert(x === Fork(Leaf('e', 2), Fork(Leaf('h', 1), Leaf('l', 2), List('h', 'l'), 3), List('e', 'h', 'l'), 5))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t2, List(1, 0, 0, 0, 0)) === "daa".toList)
    }
    println (decode(frenchCode,secret))
  }

  test("encode") {
    new TestTrees {
      assert(encode(t2)("daa".toList) === "10000".map(_.toString.toInt))
    }
  }
  test("quick encode") {
    new TestTrees {
      quickEncode(t2)("daa".toList)
      assert(quickEncode(t2)("daa".toList) === "10000".map(_.toString.toInt))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }

  }

  test("convert codetable") {
    new TestTrees {
      println(convert(t2)===List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }
}
