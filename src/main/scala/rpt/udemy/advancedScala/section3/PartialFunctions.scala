package rpt.udemy.advancedScala.section3

import scala.util.Try

object PartialFunctions extends App{

  object FunctionNotApplicableException extends RuntimeException

  val fussyFunc = (x:Int) => {
    if x==1 then 43 else throw FunctionNotApplicableException
  }

  // Partial function from {1,2} to Int
  val nicerFussyFunc = (x:Int) => x match {
    case 1 => 43
    case 2 => 292
  }

  // Sugar for previous version
  val partialFunc : PartialFunction[Int, Int] = {
    case 1 => 43
    case 2 => 292
  }

  // Will print 292
  Try {
    nicerFussyFunc(2)
  }.foreach(println)

  // Wont print
  Try {
    partialFunc(3)
  }.foreach(println)

  /*
    Partial Function's Utilites
   */

  // 1. isDefined
  println(partialFunc.isDefinedAt(3))
  // 2. Lifting turns output into options
  val lifted: Int => Option[Int] = partialFunc.lift
  // This wont print
  lifted(3).foreach(println)
  // This will
  lifted(2).foreach(println)

  // 3. Adding more cases on the spot
  val chained = partialFunc.orElse[Int, Int] {
    case x if x<100 => -1
  }
  // This should still print
  chained.lift(2).foreach(println)
  // This now should print too
  chained.lift(3).foreach(println)

  // 4. Partial functioin extend total functions
  val foo : Int => Int = {
    case 1 => 23
  }

  // therefore HOF accept also partial functions
  val mappedList = List(1,2,3,4) map {
    case 1 => 123
    case 2 => 393
    case 3 => 93
    case 4 => 39
  }

  /*
   ## NOTE: Only one parameter allowed
  */


  /**
   * EXERCISES:
   *
   * 1. Instantiate on the spot a partial function
   * 2. ChatBot
   */

  val onTheSpot = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 10
    }

    override def isDefinedAt(x: Int): Boolean = Try {
      apply(x)
    }.isSuccess
  }

  val chatBot : String => String = {
    case "Hello" => "Hi"
    case "Good Bye" => "See you latter!"
    case q if q.endsWith("?") => "I don't know, sorry."
    case _ => "I see.."
  }

  scala.io.Source.stdin.getLines() map chatBot foreach println

}
