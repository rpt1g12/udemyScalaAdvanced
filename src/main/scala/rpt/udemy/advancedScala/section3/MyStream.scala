package rpt.udemy.advancedScala.section3

import scala.annotation.tailrec

/**
 * Stream of elements of type U
 *
 * @tparam U Type of the elements of the stram.
 */
trait MyStream[+U] {

  /**
   * Indicates if the stream is empty.
   *
   * @return True if the stream is empty; false otherwise.
   */
  def isEmpty: Boolean

  /**
   * First element of the stream.
   *
   * @return First element of the stream.
   */
  def head: U

  /**
   * Returns the remaining elements of the stream
   *
   * @return Remaining elements of the stream
   */
  def tail: MyStream[U]

  /**
   * Prepends an element to the stream.
   *
   * @param e Element to prepend to the stream
   * @tparam V Type of the element to be prepended.
   * @return New stream with the element given as head.
   */
  def #::[V >: U](e: V): MyStream[V]

  /**
   * Concatenates two streams
   *
   * @param other Other stream to concatenate
   * @tparam V Type of the other stream to concatenate.
   * @return Concatenation of the two streams.
   */
  def ++[V >: U](other: => MyStream[V]): MyStream[V]

  /**
   * Applies a given function to all elements of the set.
   *
   * @param func Function to apply to each element of the set.
   */
  def forEach(func: U => Unit): Unit

  /**
   * Applies a function to each element of the original function and returns the results as a new stream.
   *
   * @param mapFunction Function to map elements from A to B
   * @tparam V Type of the new elements resulting from the transformation.
   * @return A new stream with all elements of the original set transformed by the mapFunction.
   */
  def map[V](mapFunction: U => V): MyStream[V]

  /**
   * Applies a function (that returns streams) to each element of the original function and returns the union of the
   * results as a new stream.
   *
   * @param flatMapFunction Function to map elements from U to MyStream[V]
   * @tparam V Type of the new elements resulting from the transformation.
   * @return A new stream with all elements of the original stream transformed by the flatMapFunction.
   */
  def flatMap[V](flatMapFunction: U => MyStream[V]): MyStream[V]

  /**
   * Filters out all elements of the stream that do not pass the predicate.
   *
   * @param predicate Predicate to test against all elements.
   * @return A new stream with all elemnts that pass the given predicate.
   */
  def filter(predicate: U => Boolean): MyStream[U]

  /**
   * Takes the firsts n elements of the stream
   *
   * @param n Number of elements to evaluate.
   * @return A stream with the first n elements of the stream.
   */
  def take(n: Int): MyStream[U]

  /**
   * Takes the firsts n elements of the stream and evaluates them into a list.
   *
   * @param n Number of elements to evaluate.
   * @return A list with the first n elements of the stream.
   */
  def takeAsList(n: Int): List[U] = take(n).toList

  /**
   * Converts the stream to a List
   * @return Stream evaluated into a List
   */
  final def toList: List[U] = toList(Nil)

  @tailrec
  private final def toList[V>:U](cum: List[V] = Nil) :List[V] = {
    val newCum = head :: cum
    if tail.isEmpty then newCum.reverse
    else tail.toList(newCum)
  }
}

object MyStream {
  /**
   * Creates a stream from an initial element and a funcion to generate the remaining elements
   * from the previous ones.
   *
   * @param start     First element of the stream.
   * @param generator Function to generate new elements from previous ones.
   * @tparam U Type of the elements of the stream.
   * @return A stream with the given initial element and the remaining generated from it.
   */
  def from[U](start: U)(generator: U => U): MyStream[U] = new LazyCons[U](
    start,
    from(generator(start))(generator)
  )
}

private class LazyCons[+U](override val head: U, t: => MyStream[U]) extends MyStream[U] {
  override lazy val tail: MyStream[U] = t

  override def isEmpty: Boolean = false

  override def #::[V >: U](e: V): MyStream[V] = new LazyCons[V](e, this)

  override def ++[V >: U](other: => MyStream[V]): MyStream[V] = {
    new LazyCons[V](head, tail ++ other)
  }

  def forEach(func: U => Unit): Unit = {
    func(head)
    if !tail.isEmpty then {
      tail.forEach(func)
    }
  }

  def map[V](mapFunction: U => V): MyStream[V] = new LazyCons[V](mapFunction(head), tail.map(mapFunction))

  def flatMap[V](flatMapFunction: U => MyStream[V]): MyStream[V] = {
    flatMapFunction(head) ++ tail.flatMap(flatMapFunction)
  }

  def filter(predicate: U => Boolean): MyStream[U] = {
    if predicate(head) then {
      new LazyCons[U](head, tail.filter(predicate))
    } else {
      tail.filter(predicate)
    }
  }

  def take(n: Int): MyStream[U] = {
    if (n <= 0) {
      EmptyStream
    } else if (n == 1) {
      new LazyCons[U](head, EmptyStream)
    } else {
      new LazyCons[U](head, tail.take(n - 1))
    }
  }

}

object EmptyStreamException extends IllegalArgumentException("Empty stream has no elements.")

object EmptyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw EmptyStreamException

  override def tail: MyStream[Nothing] = EmptyStream

  override def #::[V >: Nothing](e: V): MyStream[V] = new LazyCons[V](e, EmptyStream)

  override def ++[V >: Nothing](other: => MyStream[V]): MyStream[V] = other

  override def forEach(func: Nothing => Unit): Unit = ()

  override def map[V](mapFunction: Nothing => V): MyStream[V] = EmptyStream

  override def flatMap[V](flatMapFunction: Nothing => MyStream[V]): MyStream[V] = EmptyStream

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = EmptyStream

  override def take(n: Int): MyStream[Nothing] = EmptyStream
}
