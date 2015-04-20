package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.util._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("melding empty heaps returns empty") = forAll { (h: H) =>
    isEmpty(meld(empty, empty))
  }

  property("min of 2 on a heap") = forAll { (a: Int, b: Int) =>
    val heap = insert(b, insert(a, empty))
    val min = Math.min(a, b)
    findMin(heap) == min
  }

  property("delete minimum of a single-item heap returns empty") = forAll { (a: Int) =>
    val heap = insert(a, empty)
    val h = deleteMin(heap)
    isEmpty(h)
  }

  property("melding heaps minimum") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    val min1 = findMin(melded)
    val min2 = Math.min(findMin(h1), findMin(h2))
    min1 == min2
  }

  def sortHeap(heap: H): Seq[A] = sortHeap(List[A](), heap)

  def sortHeap(seq: Seq[A], heap: H): Seq[A] =  {
    if (isEmpty(heap)) seq
    else {
      val min = findMin(heap)
      sortHeap(seq :+ min, deleteMin(heap))
    }
  }

  property("extracting min returns sorted collection") = forAll { (h: H) =>
    assertSortOrder(h)
  }

  def assertSortOrder(h: H): Boolean = {
    val sorted1 = sortHeap(h)
    val sorted2 = sorted1.sorted
    sorted1 equals sorted2
  }

  property("melding has all elements") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val set1 = sortHeap(h1).toSet
    val set2 = sortHeap(h2).toSet
    val setH = sortHeap(h).toSet
    set1.union(set2).diff(setH).isEmpty
  }

  lazy val genHeap: Gen[H] = for
  {
      item <- arbitrary[A]
      //heap <- frequency((1, empty), (9, genHeap))
      heap <- oneOf(const(empty), genHeap)
  } yield insert(item, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
