package rpt.udemy.advancedScala.section4

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

object SocialNetworkApp extends App {

  object SocialNetwork {
    private val names = Map(0 -> "Dummy", 1 -> "Mark", 2 -> "Bill")

    private val friends = Map(1 -> 2)

    private lazy val rgn = Random(0)

    private def work = Thread.sleep(rgn.between(1, 4) * 500)

    def fetchProfile(id: Int): Future[Profile] = Future {
      println("Fetching profile...")
      work
      names.get(id).map(name => Profile(id, name)).get
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      println("Fetching best friend...")
      work
      friends.get(profile.id).flatMap(friendId => names.get(friendId).map(friendName => Profile(friendId, friendName))).get
    }
  }

  case class Profile(id: Int, name: String) {
    def poke(other: Profile): Unit = {
      println(s"${this.name} poking ${other.name}")
    }
  }

  val markFuture = SocialNetwork.fetchProfile(1)
  markFuture.onComplete { case Success(mark) => {
    val billFuture = SocialNetwork.fetchBestFriend(mark)
    billFuture.onComplete { case Success(bill) => mark.poke(bill)
    case Failure(exception) => exception.printStackTrace
    }
  }
  case Failure(exception) => exception.printStackTrace
  }

//  SocialNetwork.fetchProfile(1).foreach(mark => SocialNetwork.fetchBestFriend(mark).foreach(bill => mark.poke(bill)))

  for {
    mark <- SocialNetwork.fetchProfile(1)
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)

  // Fallbacks
  val profileNoMatterWhat = SocialNetwork.fetchProfile(3).recover {
    case e: Throwable => Profile(-1, "Empty")
  }
  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile(3).recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile(0)
  }
  val fallbackResult = SocialNetwork.fetchProfile(3).fallbackTo(SocialNetwork.fetchProfile(0))

  Thread.sleep(2000)
}
