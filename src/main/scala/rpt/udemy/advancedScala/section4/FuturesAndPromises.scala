package rpt.udemy.advancedScala.section4

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.impl.Promise
import scala.util.{Failure, Random, Success}

object FuturesAndPromises extends App {

  def meaningOfLife: Int = {
    Thread.sleep(200)
    42
  }

  val future1 = Future {
    meaningOfLife
  }

  future1.onComplete {
    case Success(value) => println(s"The meaning of life is ${value}")
    case Failure(exception) => println(s"Failed with:\n${exception}")
  }

  /**
   * Example of mini Social Net
   */

  case class Profile(id: String, name: String) {
    def poke(other: Profile): Unit = {
      println(s"${this.name} is poking on ${other.name}'s profile'")
    }
  }

  object SocialNetwork {
    private val rgn = Random

    def longComputation: Unit = {
      Thread.sleep(rgn.nextInt(200))
    }

    val profiles = Map(
      "0" -> "Dummy",
      "1" -> "Mark",
      "2" -> "Bill"
    )
    val friends = Map(
      "1" -> Seq("2"),
      "2" -> Seq("1")
    )

    def fetchProfile(id: String): Future[Profile] = Future {
      longComputation
      Profile(id, profiles(id))
    }

    def fetchBFs(profile: Profile): Future[Seq[Profile]] = Future {
      longComputation
      friends(profile.id).map(id => Profile(id, profiles(id)))
    }

  }

  // We want Mark to poke Bill
  // Method 1:
  {
    val mark = SocialNetwork.fetchProfile("1")
    mark.onComplete {
      case Success(markProfile) =>
        val bfs = SocialNetwork.fetchBFs(markProfile)
        bfs.onComplete {
          case Success(profiles) => profiles.foreach(markProfile.poke)
          case Failure(exception) => exception.printStackTrace()
        }
      case Failure(exception) => exception.printStackTrace()
    }
  }

  // Method 2:
  {
    for {
      mark <- SocialNetwork.fetchProfile("1")
      bfs <- SocialNetwork.fetchBFs(mark)
    } bfs.foreach(mark.poke)
  }


  /**
   * Fallbacks
   */

  // Recover in case of fail
  val aProfile4Sure = SocialNetwork.fetchProfile("") recover {
    case exception: Exception => Profile("-1", "NoProfile")
  }
  // If it fails on the second fetch ir returns the fail of the first one
  val aFetchedProfile4Sure = SocialNetwork.fetchProfile("") recoverWith {
    case exception: Exception => SocialNetwork.fetchProfile("0")
  }
  val fallBackProfile = SocialNetwork.fetchProfile("") fallbackTo {
    SocialNetwork.fetchProfile("0")
  }

  /**
   * Promises
   *
   * 1. Return fastest future
   * 2. Return slowest future
   * 3. Repeat until succeed (condition is met)
   */

  val fastFuture = Future {
    Thread.sleep(300)
    "Fast"
  }

  val slowFuture = Future {
    Thread.sleep(600)
    "Slow"
  }

  def fastest[A](futures: Future[A]*): Future[A] = {
    val promise: Promise[A] = Promise()
    futures.foreach(_.onComplete(promise.tryComplete))
    promise.future
  }

  def slowest[A](futures: Future[A]*): Future[A] = {
    val all: Promise[A] = Promise()
    val last: Promise[A] = Promise()
    futures.foreach { f =>
      f.onComplete { t =>
        if !all.tryComplete(t) then f.onComplete(last.tryComplete)
      }
    }
    last.future
  }

  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] = {
    action().flatMap { v =>
      if condition(v) then Future(v)
      else retryUntil(action,condition)
    }
  }


  fastest(fastFuture, slowFuture).foreach(println)
  slowest(fastFuture, slowFuture).foreach(println)

  val rgn = Random(System.currentTimeMillis)
  val act: () => Future[Int] = () => Future {
    val v = rgn.nextInt(100)
//    Thread.sleep(v*2)
    println(s"Generated: ${v}")
    v
  }
  val cond: Int => Boolean = v => {
    if v <=10 then {
      println(s"Success! result = ${v}")
      true
    } else {
      println("Iterating...")
      false
    }
  }
  retryUntil(
    action = act ,
    condition = cond
  ).foreach(println)

  Thread.sleep(2000)
}
