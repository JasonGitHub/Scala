package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  property("findMin") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    findMin(h) == ord.min(a1, a2)
  }

  property("deleteMin") = forAll { (a1: Int, a2: Int, a3: Int) =>
    val h = insert(a3, insert(a2, insert(a1, empty)))
    findMin(deleteMin(deleteMin(h))) == ord.max(ord.max(a1, a2), a3)
  }
  
  property("link") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    findMin(deleteMin(h)) == ord.max(a1, a2)
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == ord.min(findMin(h1), findMin(h2))
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}