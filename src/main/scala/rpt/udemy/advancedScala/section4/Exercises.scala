package rpt.udemy.advancedScala.section4

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success}

object Exercises extends App {
  // 1. Fulfill future IMMEDIATELY
  def fulfillImmediatelly[T](v:T):Future[T] = Future.successful(v)
  val fast = fulfillImmediatelly(1)

  // 2. In sequence
  def inSequence[A,B](fa: Future[A], fb: Future[B]): Future[B] = {
    fa.flatMap(_ => fb)
  }

  // 3. First
  def first[T](fa:Future[T],fb:Future[T]):Future[T] = {
    val firstPromise = Promise[T]
    fa.onComplete(firstPromise.tryComplete)
    fb.onComplete(firstPromise.tryComplete)
    firstPromise.future
  }
  // 4. Last
  def last[T](fa:Future[T], fb:Future[T]):Future[T] = {
    val firstPromise = Promise[T]
    val secondPromise = Promise[T]
    fa.onComplete { t =>
      if (!firstPromise.tryComplete(t)) {
        secondPromise.complete(t)
      }
    }
    fb.onComplete { t =>
      if (!firstPromise.tryComplete(t)) {
        secondPromise.complete(t)
      }
    }
    secondPromise.future
  }
  // 4. RetryUntil condition is met
  def retryUntil[T](f: => () => Future[T], cond: T => Boolean):Future[T] = {
    f().filter(cond).recoverWith {
      case _ => retryUntil(f,cond)
    }
  }

  val fastFuture =  Future{
    Thread.sleep(500)
    "I'm fast!"
  }
  val slowFuture =  Future{
    Thread.sleep(800)
    "I'm slow!"
  }

  val rgn = Random(0)
  val randomNumbers = () => Future {
    val i = rgn.nextInt(100)
    println(s"Generated $i")
    i
  }

  // -- TRY FUNCTIONS --
  inSequence(slowFuture,fastFuture).foreach(r=>println(s"In seq: $r"))
  first(fastFuture, slowFuture).foreach(r=>println(s"First: $r"))
  last(fastFuture, slowFuture).foreach(r=>println(s"Last: $r"))
  retryUntil(randomNumbers, _<10).foreach(i => println(s"Settled at $i"))

  Thread.sleep(2000)


}
