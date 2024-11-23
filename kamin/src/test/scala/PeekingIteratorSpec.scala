package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PeekingIteratorSpec extends AnyFunSpec with Matchers {

  describe("A PeekingIterator") {

    it("should return elements in the correct order without skipping any") {
      val iter = Iterator(1, 2, 3, 4, 5)
      val peekIter = new PeekingIterator(iter)

      peekIter.next() should be (1)
      peekIter.next() should be (2)
      peekIter.next() should be (3)
    }

    it("should allow peeking at elements without advancing the iterator") {
      val iter = Iterator(1, 2, 3, 4, 5)
      val peekIter = new PeekingIterator(iter)

      peekIter.peek(2) should be (List(1, 2)) // Peeking should not advance the iterator
      peekIter.next() should be (1)           // Now actually consume the first element
      peekIter.next() should be (2)           // Consume the second element
      peekIter.next() should be (3)           // Consume the third element - the first from the iterator
    }

    it("should allow multiple peeks without consuming the iterator") {
      val iter = Iterator(1, 2, 3, 4, 5)
      val peekIter = new PeekingIterator(iter)

      peekIter.peek(3) should be (List(1, 2, 3))  // Peeking ahead 3 elements
      peekIter.peek(2) should be (List(1, 2))     // Still able to peek only 2 elements
      peekIter.next() should be (1)               // Now consuming elements
      peekIter.peek(2) should be (List(2, 3))     // Peeking the next 2 elements
    }

    it("should return true for hasNext when there are remaining elements") {
      val iter = Iterator(1, 2, 3)
      val peekIter = new PeekingIterator(iter)

      peekIter.hasNext should be (true) // There are elements available
      peekIter.next() should be (1)     // Consume an element
      peekIter.hasNext should be (true) // Still elements remaining
      peekIter.next() should be (2)
      peekIter.next() should be (3)
      peekIter.hasNext should be (false) // No more elements
    }

    it("should handle peeking more elements than are available in the iterator") {
      val iter = Iterator(1, 2)
      val peekIter = new PeekingIterator(iter)

      peekIter.peek(5) should be (List(1, 2))  // Peeking more than available
      peekIter.hasNext should be (true)        // Iterator is not yet exhausted
      peekIter.next() should be (1)            // Consume the first element
      peekIter.next() should be (2)            // Consume the second element
      peekIter.hasNext should be (false)       // Now iterator is exhausted
    }

    it("should handle empty iterator correctly") {
      val iter = Iterator.empty[Int]
      val peekIter = new PeekingIterator(iter)

      peekIter.hasNext should be (false)
      peekIter.peek(1) should be (List())      // Peeking on empty iterator should return empty list
    }
  }
}
