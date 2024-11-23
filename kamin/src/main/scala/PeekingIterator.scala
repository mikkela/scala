package kamin

import scala.collection.mutable

class PeekingIterator[A](iter: Iterator[A]) extends Iterator[A]:
  private var buffer: List[A] = List.empty

  // Peek at the next `n` elements without consuming them
  def peek(n: Int): List[A] =
    if n <= buffer.size then
      buffer.take(n)
    else 
      val elementsToFetch = n - buffer.size
      val newElements = iter.take(elementsToFetch).toList
      buffer = buffer ++ newElements
      buffer.take(n)

  def consumeTokens(n: Int): Unit =
    (1 to n).foreach(_ => next())

  // Has next only if there are elements in the buffer or the original iterator
  override def hasNext: Boolean = buffer.nonEmpty || iter.hasNext

  // Return the next element, consuming the buffer if necessary
  override def next(): A = 
    if buffer.nonEmpty then
      val head = buffer.head
      buffer = buffer.tail
      head
    else 
      iter.next()


