package rpt.udemy.advancedScala.section4

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.{Failure, Random, Success}

object OnlineBankingApp extends App {

  case class User(name: String)

  case class Transaction(sender: User, receiver: String, amount: Int, status: String)

  object BankingApp {
    val name = "MyBank"
    private lazy val rgn = Random(0)
    private def work(s:String) = {
      println(s)
      Thread.sleep(rgn.between(1, 4) * 500)
    }

    private def fetchUser(name:String): Future[User] = Future {
      work("Fetching User...")
      User(name)
    }

    private def createTransaction(user:User, merchant:String, amount:Int): Future[Transaction] = Future {
      work("Creating transaction...")
      Transaction(user,merchant,amount,"SUCCESS")
    }

    def purchase(userName:String, item:String, merchant:String, cost:Int): String = {
      println(s"$userName attempting to buy $item from $merchant for $cost$$")
      val futureStatus = for {
        user <- fetchUser(userName)
        transaction <- createTransaction(user,merchant,cost)
      } yield transaction.status

      Await.result(futureStatus,2000.milliseconds)
    }
  }

  println {
    BankingApp.purchase(
      "Rafa", "Motorbike", "Honda", 3000
    )
  }

  // PROMISES
  val promise = Promise[Int]()
  val future = promise.future

  // Thread-1: Consumer
  future.onComplete {
    case Failure(exception) => exception.printStackTrace
    case Success(value) => println(s"[consumer] consumed $value")
  }

  // Thread-2: Producer
  val producer = new Thread( () => {
    println("[producer] Working...")
    Thread.sleep(500)
    promise.success(8)
    println("[producer] Done!")
  })

  producer.start()

  Thread.sleep(1000)

}
