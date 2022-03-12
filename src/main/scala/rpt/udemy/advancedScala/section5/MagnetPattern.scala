package rpt.udemy.advancedScala.section5

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MagnetPattern extends App {

  // Magnet patterns solves method overloading
  class P2PRequest
  class P2PResponse
  class Serializer[T]

  trait Actor {
    def receive(statusCode: Int): Int
    def receive(response: P2PRequest): Int
    def receive[T: Serializer](message: T): Int
    def receive[T: Serializer](message: T, statusCode: Int): Int
    def receive(future:Future[P2PRequest]):Int
    // lots of overloads
//    def receive(future:Future[P2PRequest]):Int // does not compile because of type erasure
    /*
      Problems:
        1. Type erasure
        2. Lifting does not work for all overloads
          val receiveFunc = Actor.receive _ // wich _ Int, P2PRequest, ... ?
        3. Code duplication. Most method's implementation maybe similar
        4. Type inference and default args
          Actor.receive(??) // what default
    */
  }

  trait MessageMagnet[Result] {
    def apply():Result
  }

  object BetterActor {
    def receive[R](magnet:MessageMagnet[R]): R = magnet()
    def liftedReceive(magnet:LiftingMagnet): Int = magnet()
  }

  implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int] {
    // Implicit conversion
    override def apply(): Int = {
      // here we implement logic that would go into receive method with P2PRequest param
      println("P2PRequest logic")
      42
    }
  }

  implicit class FromP2PResponse(response: P2PResponse) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // here we implement logic that would go into receive method with P2PRequest param
      println("P2PResponse logic")
      42
    }
  }

  BetterActor.receive(new P2PRequest)
  BetterActor.receive(new P2PResponse)

  // Benefits:
  // 1. No more type erasure problems because implicits are looked fore before types are erased
  implicit class FromFutureP2PRequest(request: Future[P2PRequest]) extends MessageMagnet[Int] {
    // Implicit conversion
    override def apply(): Int = {
      // here we implement logic that would go into receive method with P2PRequest param
      println("Future P2PRequest logic")
      42
    }
  }
  implicit class FromFutureP2PResponse(response: Future[P2PResponse]) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // here we implement logic that would go into receive method with P2PRequest param
      println("Future P2PResponse logic")
      42
    }
  }

  BetterActor.receive{
    Future(new P2PRequest)
  }
  BetterActor.receive{
    Future(new P2PResponse)
  }

  // 2. Lifting with the following magnet (without type) and returns method type
  trait LiftingMagnet {
    def apply():Int
  }

  implicit class LiftingStringMagnet(value:String) extends LiftingMagnet {
    override def apply(): Int = {
      println("String logic")
      value.toInt
    }
  }
  implicit class LiftingDoubleMagnet(value:Double) extends LiftingMagnet {
    override def apply(): Int = {
      println("Double logic")
      value.toInt
    }
  }

  val foo = BetterActor.liftedReceive _

  foo("1")
  foo(1.0)
  BetterActor.liftedReceive(1f) // implicit conversion to double
  BetterActor.liftedReceive(1) // implicit conversion to double

  /*
    Problems:
      1. Very verbose
      2. Hard to read
      3. You cannot use default arguments (or no arguments)
      4. Call by name does not work correctly:
        Only works if functions are properly defined. Not with anonymous
  */

  trait HandlerMagnet {
    def apply():Unit
  }

  object Handler {
    def handle(magnet: HandlerMagnet):Unit = magnet()
  }

  implicit class StringHandler(value: => String) extends HandlerMagnet {
    override def apply(): Unit = {
      println(value)
      println(value)
    }
  }

  def sideEffectFn:String = {
    println("Side effect")
    "A String"
  }


  Handler.handle(sideEffectFn) // This prints twice the side effect call to print
  Handler.handle {
    println("Side effect")  // This only prints once
    "A String" // This is what is implicitly converted to HandlerMagnet
  }

}
