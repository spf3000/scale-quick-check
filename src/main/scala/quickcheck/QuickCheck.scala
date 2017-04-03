package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(QuickCheckHeap.this.empty),
    for {
      k <- arbitrary[A]
      m <- oneOf(const(QuickCheckHeap.this.empty), genHeap)
  }  yield insert(k,m)
  )


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("min of 1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get the smallest of the two elements back.
    */
  property("min of 2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    a.min(b) == findMin(h)
  }

  property("min of 3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    a.min(b.min(c)) == findMin(h)
  }

  /**
    * If you insert an element into an empty heap,
    * then delete the minimum, the resulting heap should be empty.
    */
  property("empty=>ins=>deleteMin isEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("empty isEmpty") = forAll { a: Int =>
    isEmpty(empty)
  }

  property("is not empty for inserted") = forAll { a: Int =>
    !isEmpty(insert(a, empty))
  }

  property("delete min of single element is empty") = forAll {a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }
  property("deleteMin of 2 element heap") = forAll{(a: Int, b: Int) =>
    deleteMin(insert(a,insert(b,empty))) == insert(a.max(b),empty)
  }
  property("deleteMin of 3 element heap") = forAll{(a: Int, b: Int, c: Int) =>
    deleteMin(insert(c,insert(a,insert(b,empty)))) == insert(a.max(b),empty)
  }

  property("delete min and insert it again") =forAll { (h: H) =>
    if (!isEmpty(h)){
      val r = insert(findMin(h),deleteMin(h))
      findMin(r) == findMin(h)
    }
    else true
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("sequence") = forAll{ h: H =>
      def loop(list: List[Int], heap: H): List[Int] ={
        if (isEmpty(heap)) List.empty
        else findMin(heap) :: loop( list , deleteMin(heap))
    }
    val l = loop(List.empty, h)
    l.sorted == l
  }
  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("min(meld(a,b) == min a or min b") = forAll { (a: H, b: H) =>
    val am = if (isEmpty(a)) 0 else findMin(a)
    val bm = if (isEmpty(b)) 0 else findMin(b)
    val m = meld(a, b)
    val theMin = if (isEmpty(m)) 0 else findMin(m)
    theMin == am || theMin == bm
  }

  /**
    * Finding a minimum of the melding of any heap with an empty
    */
  property("min(meld(a,empty) == min a") = forAll { (a: H) =>
    val am = if (isEmpty(a)) 0 else findMin(a)
    val m = meld(a, empty)
    val theMin = if (isEmpty(m)) 0 else findMin(m)
    theMin == am
  }

  property("meld of 2 empties is empty") = forAll { a: Int =>
    isEmpty(meld(empty, empty))
  }


}














