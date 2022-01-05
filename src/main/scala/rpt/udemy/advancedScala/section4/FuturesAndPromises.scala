package rpt.udemy.advancedScala.section4

import scala.concurrent.ExecutionContext.global

object FuturesAndPromises extends App {

  def meaningOfLife:Int = {
    Thread.sleep(2000)
    42
  }

  val future1 = Future {
    meaningOfLife
  }


}
