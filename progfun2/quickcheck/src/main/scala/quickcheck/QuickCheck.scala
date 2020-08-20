package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      v <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(v, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (if (a > b) b else a)
  }

  property("empty1") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }
  property("min3") = forAll { (h1: H, h2: H) =>
    val a = findMin(h1)
    val b = findMin(h2)
    findMin(meld(h1, h2)) == (if (a > b) b else a)
  }

  property("sortSeq") = forAll { h: H =>
    def helper(heap: H, acc: Int): Boolean = {
      if (isEmpty(heap)) true
      else if (findMin(heap) > acc) helper(deleteMin(heap), findMin(heap))
      else false
    }
    helper(deleteMin(h), findMin(h))
  }


}
