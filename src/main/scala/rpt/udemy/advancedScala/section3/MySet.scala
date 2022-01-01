package rpt.udemy.advancedScala.section3

import java.util.logging.{Level, Logger}
import scala.annotation.tailrec
import scala.util.Random

/**
 * Custom Implementation of a functional set
 *
 * @tparam A Type of the elements contained in the set
 */
trait MySet[A] extends (A => Boolean) {

  protected val LOG: Logger = Logger.getLogger(this.getClass.getCanonicalName)

  override def apply(e: A): Boolean = contains(e)

  def head: A

  def tail: MySet[A]

  /**
   * Indicates if an element is contained in the set.
   *
   * @param e Element to check if is part of the set.
   * @return True if the element is contained; false otherwise.
   */
  def contains(e: A): Boolean

  /**
   * Returns the complement of a set.
   */
  def unary_! : MySet[A]

  /**
   * Adds an element in to the set if its not already present.
   *
   * @param e Element to add to the set.
   * @return New set with the added element.
   */
  def +(e: A): MySet[A]

  /**
   * Union of two sets.
   *
   * @param other Other set to perfrom the union operation.
   * @return A new set with all elements from both sets.
   */
  def ++(other: MySet[A]): MySet[A]

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

  /**
   * Removes a given element from the set.
   *
   * @param e Element to remove
   * @return Set without the given element.
   */
  def - (e: A): MySet[A]

  /**
   * Returns the intersection between this set and another.
   *
   * @param other The other set to intersect this with.
   * @return Intersection of the two sets where the resulting set only contains elements present in both sets.
   */
  def &(other: MySet[A]): MySet[A]

  /**
   * Returns the difference between this set and another.
   *
   * @param other Other set to remove from this.
   * @return Returns the difference of the two sets where the resulting set contains elements only present in this set.
   */
  def --(other: MySet[A]): MySet[A]

  override def toString(): String = strRepr

  def strRepr: String
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

    helper(new Empty[A], elements)
  }
}

object EmptySetException extends RuntimeException("Trying to acces elements of an empty set.")
object InfiniteSetException extends RuntimeException("Trying to acces elements of the infinite set.")

class Complement[A](cons:MySet[A]) extends MySet[A] {

  override def head: Nothing = throw InfiniteSetException

  override def tail: MySet[A] = this

  override def isEmpty: Boolean = false

  override def contains(e: A): Boolean = !cons(e)

  override def unary_! : MySet[A] = cons

  override def +(e: A): MySet[A] = {
    if cons contains e then
      new Complement[A](cons - e)
    else {
      this
    }
  }

  override def ++(other: MySet[A]): MySet[A] = new Complement[A](cons -- other)

  override def map[B](mapFunction: A => B): MySet[B] = new Empty[B]

  override def flatMap[B](flatMapFunction: A => MySet[B]): MySet[B] = new Empty[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def forEach(f: A => Unit): Unit = {}

  override def -(e: A): MySet[A] = new Complement[A](cons+e)

  override def &(other: MySet[A]): MySet[A] = other -- cons

  override def --(other: MySet[A]): MySet[A] = new Complement[A](cons ++ other)

  override def strRepr: String = s"!$cons"
}

class Empty[A] extends MySet[A] {
  override def head: Nothing = throw EmptySetException

  override def tail: MySet[A] = new Empty[A]

  override def isEmpty: Boolean = true

  override def contains(e: A): Boolean = false

  override def unary_! : MySet[A] = new Complement[A](this)

  override def +(e: A): MySet[A] = Cons(e, this)

  override def ++(other: MySet[A]): MySet[A] = other

  override def map[B](mapFunction: A => B): MySet[B] = new Empty[B]

  override def flatMap[B](flatMapFunction: A => MySet[B]): MySet[B] = new Empty[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def forEach(f: A => Unit): Unit = {}

  override def -(e: A): MySet[A] = this

  override def &(other: MySet[A]): MySet[A] = this

  override def --(other: MySet[A]): MySet[A] = this

  override def strRepr: String = "{}"
}

case class Cons[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(e: A): Boolean = {
    if (head == e) {
      true
    } else {
      tail.contains(e)
    }
  }

  override def unary_! : MySet[A] = new Complement[A](this)

  override def isEmpty: Boolean = false

  override def +(e: A): MySet[A] = {
    if contains(e) then this else Cons(e, this)
  }

  override def ++(other: MySet[A]): MySet[A] = {
    @tailrec
    def helper(cum: MySet[A], rem: MySet[A]): MySet[A] = {
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

    helper(cum = new Empty[B], this)
  }

  override def flatMap[B](flatMapFunction: A => MySet[B]): MySet[B] = {
    @tailrec
    def helper(cum: MySet[B], rem: MySet[A]): MySet[B] = {
      if (rem.isEmpty) {
        cum
      } else {
        val mapped = flatMapFunction(rem.head)
        helper(cum ++ mapped, rem.tail)
      }
    }

    helper(cum = new Empty[B], this)
  }

  override def filter(predicate: A => Boolean): MySet[A] = {
    @tailrec
    def helper(cum: MySet[A], rem: MySet[A]): MySet[A] = {
      if (rem.isEmpty) {
        cum
      } else {
        if (predicate(rem.head)) {
          helper(cum + rem.head, rem.tail)
        } else {
          helper(cum, rem.tail)
        }
      }
    }

    helper(cum = new Empty[A], this)
  }

  override def forEach(func: A => Unit): Unit = {
    func(head)
    tail.forEach(func)
  }

  override def strRepr: String = {
    @tailrec
    def helper(acc: Seq[String], rem: MySet[A]): Seq[String] = {
      if rem.isEmpty then acc
      else helper(acc = acc :+ rem.head.toString, rem.tail)
    }

    s"{${helper(Nil, this).mkString(", ")}}"
  }

  override def -(e: A): MySet[A] = {
    @tailrec
    def helper(rem: MySet[A], acc: MySet[A] = new Empty[A]) : MySet[A] = {
      if rem contains e then {
        if rem.head == e then {
          tail ++ acc
        } else {
          helper(rem.tail,acc+rem.head)
        }
      } else {
        rem
      }
    }
    helper(this)
  }

  override def &(other: MySet[A]): MySet[A] ={
    filter(!other)
  }

  override def --(other: MySet[A]): MySet[A] = {
    filter(e => !(other contains e))
  }
}
