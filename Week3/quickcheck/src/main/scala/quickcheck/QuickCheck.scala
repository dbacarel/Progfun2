package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      el <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(el, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("leeeerooooy jenkins") = forAll { (x:A, y:A) =>
    val minEl = ord.min(x,y)
    findMin(insert(x, insert(y, empty))) == minEl
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty
  property("Troposphere") =  forAll{(x:A) => isEmpty(deleteMin(insert(x, empty)))}

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("bumblebutt") = forAll{ (h:H) =>
    def kitty(mweo:H):List[A] = if (isEmpty(mweo)) Nil else findMin(mweo)::kitty(deleteMin(mweo))
    val flatHeap = kitty(h)
    flatHeap == flatHeap.sorted
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("cyberpunk is not dead") = forAll{ (h1:H, h2:H) =>
    val h1_min = findMin(h1)
    val h2_min = findMin(h2)
    val merged_heap = meld(h1, h2)
    findMin(merged_heap) == ord.min(h1_min, h2_min)
  }

  //The result of the melding an empty heap with a non-empty one is the latter
  property("identity funk") = forAll{ (a:A) =>
    val h = insert(a, empty)
    meld(empty, h) == h
  }

  //The result of the melding of 2 non empty heaps contains all the elements of those
  property("verdammt, du slack") = forAll{ (a:A, b:A) =>
    def kitty(mweo:H):List[A] = if (isEmpty(mweo)) Nil else findMin(mweo)::kitty(deleteMin(mweo))
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val m_h = meld(h1, h2)
    List(a,b).sorted == kitty(m_h).sorted
  }

  //deleteMin deletes the minima
  property("for-got-ten") = forAll{ (_:A) =>
    val h = insert(3,insert(2,insert(1, empty)))
    val min = findMin(h)
    ord.lt(min, findMin(deleteMin(h)))
  }


}
