package rpt.udemy.advancedScala.section4

import scala.collection.mutable

case class Container(size: Int) {
  private val stack = mutable.Stack[Int]()

  def isEmpty: Boolean = stack.isEmpty

  def hasCapacity: Boolean = stack.size < size

  def isFull: Boolean = stack.size == size

  def push(v: Int): Unit = stack.push(v)

  def pop: Int = stack.pop()

}
