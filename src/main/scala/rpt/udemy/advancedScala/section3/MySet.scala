package rpt.udemy.advancedScala.section3

import scala.annotation.tailrec

/**
 * Custom Implementation of a functional set
 *
 * @tparam A Type of the elements contained in the set
 */
trait MySet[+A] extends Function1[A,Boolean] {

  override def apply(e: A): Boolean = contains(e)

  def head: A

  def tail: MySet[A]

  /**
   * Indicates if an element is contained in the set.
   *
   * @param e Element to check if is part of the set.
   * @return True if the element is contained; false otherwise.
   */
  def contains[B >: A](e: B): Boolean

  /**
   * Adds an element in to the set if its not already present.
   *
   * @param e Element to add to the set.
   * @return New set with the added element.
   */
  def +[B >: A](e: B): MySet[B]

  /**
   * Union of two sets.
   *
   * @param other Other set to perfrom the union operation.
   * @return A new set with all elements from both sets.
   */
  def ++[B >: A](other: MySet[B]): MySet[B]

  /**
   * Applies a function to each element of the original function and returns the results as a new set.
   *
   * @param mapFunction Function to map elements from A to B
   * @tparam B Type of the new elements resulting from the transformation.
   * @return A new set with all elements of the original set transformed by the mapFunction.
   */
  def map[B](mapFunction: A => B): MySet[B]

  /**
   * Applies a function (that returns sets) to each element of the original function and returns the union of the
   * results as a new set.
   *
   * @param flatMapFunction Function to map elements from A to MyList[B]
   * @tparam B Type of the new elements resulting from the transformation.
   * @return A new set with all elements of the original set transformed by the flatMapFunction.
   */
  def flatMap[B](flatMapFunction: A => MySet[B]): MySet[B]

  /**
   * Filters out all elements of the set that do not pass the predicate.
   *
   * @param predicate Predicate to test against all elements.
   * @return A new set with all elemnts that pass the given predicate.
   */
  def filter(predicate: A => Boolean): MySet[A]

  /**
   * Indicates if the set is empty.
   *
   * @return True if the set is empty; false otherwise.
   */
  def isEmpty: Boolean

  /**
   * Applies a given function to all elements of the set.
   *
   * @param func Function to apply to each element of the set.
   */
  def forEach(func: A => Unit): Unit
}

object MySet {
  def apply[A](elements: A*): MySet[A] = {
    @tailrec
    def helper(acc: MySet[A], remaining: Seq[A]): MySet[A] = {
      if (remaining.isEmpty) {
        acc
      } else {
        helper(acc + remaining.head, remaining.tail)
      }
    }

    helper(Empty, elements)
  }
}

object EmptySetException extends RuntimeException("Trying to acces elements of an empty set.")

case object Empty extends MySet[Nothing] {
  override def head: Nothing = throw EmptySetException

  override def tail: MySet[Nothing] = Empty

  override def isEmpty: Boolean = true

  override def +[A >: Nothing](e: A): MySet[A] = Cons(e, Empty)

  override def ++[A >: Nothing](other: MySet[A]): MySet[A] = other

  override def map[A](mapFunction: Nothing => A): MySet[Nothing] = Empty

  override def flatMap[A](flatMapFunction: Nothing => MySet[A]): MySet[Nothing] = Empty

  override def filter(predicate: Nothing => Boolean): MySet[Nothing] = Empty

  override def forEach(f: Nothing => Unit): Unit = {}

  override def strRepresentation: String = ""
}

case class Cons[+A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains[B >: A](e: B): Boolean = {
    if (head == e) {
      true
    } else {
      tail.contains(e)
    }
  }

  override def isEmpty: Boolean = false

  override def +[B >: A](e: B): MySet[B] = {
    if contains(e) then this else Cons(e, this)
  }

  override def ++[B >: A](other: MySet[B]): MySet[B] = {
    @tailrec
    def helper(cum: MySet[B], rem: MySet[B]): MySet[B] = {
      if (rem.isEmpty) {
        cum
      } else {
        if (cum.contains(rem.head)) {
          cum
        } else {
          helper(cum + rem.head, rem.tail)
        }
      }
    }

    helper(this, other)
  }

  override def map[B](mapFunction: A => B): MySet[B] = {
    @tailrec
    def helper(cum: MySet[B], rem: MySet[A]): MySet[B] = {
      if (rem.isEmpty) {
        cum
      } else {
        val mapped = mapFunction(rem.head)
        if (cum.contains(mapped)) {
          cum
        } else {
          helper(cum + mapped, rem.tail)
        }
      }
    }

    helper(cum = Empty, this)
  }

  override def flatMap[B](flatMapFunction: A => MySet[B]): MySet[B] = {
    @tailrec
    def helper(cum: MySet[B], rem: MySet[A]): MySet[B] = {
      if (rem.isEmpty) {
        cum
      } else {
        val mapped = flatMapFunction(rem.head)
        if (cum.contains(mapped)) {
          cum
        } else {
          helper(cum ++ mapped, rem.tail)
        }
      }
    }

    helper(cum = Empty, this)
  }

  override def filter(predicate: A => Boolean): MySet[A] = {
    @tailrec
    def helper(cum: MySet[A], rem: MySet[A]): MySet[A] = {
      if (rem.isEmpty) {
        cum
      } else {
        if (predicate(rem.head)) {
          cum + rem.head
        } else {
          helper(cum, rem.tail)
        }
      }
    }
    helper(cum = Empty, this)
  }

  override def forEach(func: A => Unit): Unit =
    func(head)
    tail.forEach(func)
}
