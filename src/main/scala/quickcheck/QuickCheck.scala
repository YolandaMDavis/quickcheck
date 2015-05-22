package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  property("min2") = forAll { (a: Int, b:Int) =>
    val h = insert(a, empty)
    val i = insert(b, empty)
    val j = meld(h,i)
    val min = findMin(j)
    val k = deleteMin(j)
    val min2 = findMin(k)
    if(a < b) min == a && min2 == b
    else if (b < a) min == b && min2 == a
    else
      min == min2 && a == b
  }

  property("gen1") = forAll { (l: H, m: H) =>
    val h = meld(l,m)
    val a = findMin(h)
    val j = deleteMin(h)
    val b = findMin(j)
    isEmpty(j) || a != b
  }

  lazy val genHeap: Gen[H] = for{
    n <- Gen.choose(-1000000,1000000)
    h <- oneOf(const(empty),genHeap)
  } yield insert(n,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
