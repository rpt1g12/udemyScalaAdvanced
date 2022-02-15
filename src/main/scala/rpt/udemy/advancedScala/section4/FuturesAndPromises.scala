package rpt.udemy.advancedScala.section4

import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future

object FuturesAndPromises extends App {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  def meaningOfLife:Int = {
    Thread.sleep(2000)
    42
  }

  val future1 = Future {
    meaningOfLife
  }

}
