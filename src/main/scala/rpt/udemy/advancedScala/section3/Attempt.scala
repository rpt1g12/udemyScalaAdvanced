package rpt.udemy.advancedScala.section3

trait Attempt[+A] {
  def flatMap[B](flatMapFunction: A => Attempt[B]): Attempt[B]
}

object Attempt {
  def apply[A](v: => A): Attempt[A] = {
    try {
      Success(v)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

case class Success[+A](v: A) extends Attempt[A] {
  def flatMap[B](flatMapFunction: A => Attempt[B]): Attempt[B] = {
    try {
      flatMapFunction(v)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

case class Failure(e: Throwable) extends Attempt[Nothing] {
  def flatMap[B](flatMapFunction: Nothing => Attempt[B]): Attempt[B] = this
}
