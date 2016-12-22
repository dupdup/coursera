package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = Gen.oneOf(empty,insert(9,empty))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("insert") = forAll { (h: H) =>
    val h1 = insert(2, h)
    findMin(insert(1, h1)) == 1
  }
  property("deleteMin") = forAll { (h: H) =>
    val h1 = insert(1,h)
    val h2 = insert(2, h1)
    deleteMin(h2) == h1
  }
  property("meld") = forAll { (h: H) =>
    val h1 = insert(1, h)
    val h2 = insert(2, h1)
    findMin(deleteMin(deleteMin(deleteMin(meld(h1,h2))))) == 2
  }
  property("meld") = forAll { (h: H) =>
    val h1 = insert(5, h)
    val h2 = insert(2, h1)
    val h3 = insert(1, h)
    val h4 = insert(4, h3)
    (findMin(h2) min findMin(h4)) == findMin(meld(h2,h4))
  }
  property("insert") = forAll { (h: H) =>
    val h1 = insert(3, insert(2, insert(1, insert(1, insert(3, insert(3, h))))))
    findMin(h1) == 1
  }
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

}
