package rpt.udemy.advancedScala.section4

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object Exercises extends App {
  // 1. Fulfill future IMMEDIATELY
  val fast = Await.result(Future(1), 1.second)

  // 2. In sequence
  def inSequence[T](fa: Future[T], fb: Future[T]): Seq[T] = {
    val acc = mutable.Seq[T]()
    fa.onComplete { case Success(value) => {
      acc :+ value
      fb.onComplete { case Success(value) => acc :+ value
      }
    }
    }
    acc
    val a = Await[T](fa,1.second)

  // 3. First
  // 4. Last
  // 4. RetryUntil condition is met

}
