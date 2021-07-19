package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*

class HeapProperties(heapInterface: HeapInterface) extends Properties("Heap"):

  // Import all the operations of the `HeapInterface` (e.g., `empty`
  // `insert`, etc.)
  import heapInterface.*

  // Examples of properties
  property("inserting the minimal element and then finding it should return the same minimal element") = forAll {
    (heap: List[Node]) =>
      val min =
        if isEmpty(heap) then 0
        else findMin(heap)

      findMin(insert(min, heap)) == min
  }

  property("the minimum of a heap of two elements should be the smallest of the two elements") = forAll {
    (x1: Int, x2: Int) =>
      val heap = insert(x2, insert(x1, empty))
      val min  = math.min(x1, x2)

      findMin(heap) == min
  }

  property("delete minumum of heap of one element should return an empty heap") = forAll { (x: Int) =>
    val heap1: List[Node] = insert(x, empty)
    val heap0: List[Node] = deleteMin(heap1)

    heap0 == empty
  }

  property("continually finding and deleting the minimal element of a heap should return a sorted sequence") =
    // recursively traverse the heap
    def check(heap: List[Node]): Boolean =
      // if the heap is empty, or if it has just one element, we have
      // successfully finished our checks
      if isEmpty(heap) || isEmpty(deleteMin(heap)) then true
      else
        // find the minimal element
        val x1: Int = findMin(heap)

        // delete the minimal element of `heap`
        val heap2: List[Node] = deleteMin(heap)

        // find the minimal element in `heap2`
        val x2: Int = findMin(heap2)

        // check that the deleted element is smaller than the minimal element
        // of the remaining heap, and that the remaining heap verifies the
        // same property (by recursively calling `check`)
        val checked: Boolean = x1 <= x2
        checked && check(heap2)

    // check arbitrary heaps
    forAll((heap: List[Node]) => check(heap))

  property(
    "constructing heap from arbitrary sequence of integers and repeatedly finding the minimum should sort ints"
  ) =
    def insertAll(heap: List[Node], ns: List[Int]): List[Node] =
      ns.foldLeft(empty) { case (heap, value) => insert(value, heap) }

    def unfoldMin(heap: List[Node]): List[Int] =
      def loop(heap: List[Node], acc: List[Int]): List[Int] =
        if isEmpty(heap) then acc
        else loop(deleteMin(heap), findMin(heap) :: acc)

      loop(heap, List.empty).reverse
    end unfoldMin

    forAll { (ints: List[Int]) =>
      val heap           = insertAll(empty, ints)
      val shouldBeSorted = unfoldMin(heap)

      shouldBeSorted == ints.sorted
    }

  property(
    "melding two heaps and finding minimum of the melded heap should return the min of one or the other of initial heaps"
  ) =
    def hasMin(heap1: List[Node], heap2: List[Node]): List[Node] =
      if findMin(heap1) < findMin(heap2) then heap1
      else heap2

    def removeMin(heap1: List[Node], heap2: List[Node]): (List[Node], List[Node]) =
      if hasMin(heap1, heap2) == heap1 then (deleteMin(heap1), heap2)
      else (heap1, deleteMin(heap2))

    def check(heap1: List[Node], heap2: List[Node]): Boolean =
      val melded      = meld(heap1, heap2)
      val includesMin = hasMin(heap1, heap2)

      findMin(melded) == findMin(includesMin)
      && check.tupled(removeMin(heap1, heap2))

    forAll { (heap1: List[Node], heap2: List[Node]) =>
      (!isEmpty(heap1) && !isEmpty(heap2)) ==> {
        val melded = meld(heap1, heap2)

        val includesMin = hasMin(heap1, heap2)
        val mostMin     = findMin(includesMin)

        mostMin == findMin(melded)
      }
    }

  // random heap generator --- DO NOT MODIFY
  private lazy val genHeap: Gen[List[Node]] = oneOf(
    const(empty),
    for
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(v, h)
  )

  private given Arbitrary[List[Node]] = Arbitrary(genHeap)

end HeapProperties
