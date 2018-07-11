package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: A, b: A) =>
    val h = insert(a, empty)
    val m = insert(b, h)
    findMin(m) == (if (a < b) a else b)
  }

  property("insert") = forAll { (x: A) =>
    val h = insert(x, empty)
    deleteMin(h) == empty
  }

  //  property("sortSeq") = forAll { h: H =>
  //
  //    def loop(h: H): List[A] = {
  //      if (isEmpty(h)) empty
  //      val x = findMin(h)
  //      x :: loop(deleteMin(h))
  //    }
  //
  //    val list = loop(h)
  //    list == list.sorted
  //  }


  property("melding") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)

    findMin(meld(h1, h2)) == (if (min1 < min2) min1 else min2)
  }

  property("sortedHeap") = forAll { h: H =>
    def loop(h: H): Boolean = {
      if (isEmpty(h)) true
      val x = findMin(h)
      val heap = deleteMin(h)
      isEmpty(heap) || (x <= findMin(heap) && loop(heap))
    }

    loop(h)
  }

  property("melding") = forAll { (h1: H, h2: H) =>
    def equal(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && equal(deleteMin(h1), deleteMin(h2))
      }

    val meld1 = meld(h1, h2)
    val minH1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(minH1, h2))
    equal(meld1, meld2)
  }
}
