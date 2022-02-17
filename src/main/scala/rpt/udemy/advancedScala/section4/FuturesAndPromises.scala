package rpt.udemy.advancedScala.section4

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object FuturesAndPromises extends App {

  def meaningOfLife:Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    meaningOfLife
  }

  aFuture.onComplete {
    case Success(value) => println(s"Future finished with value: $value")
    case Failure(exception) => println(s"Future failed with exception: $exception")
  }

  // Wait for futures to complete
  Thread.sleep(3000)
}
