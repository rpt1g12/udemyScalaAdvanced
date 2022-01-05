package rpt.udemy.advancedScala.section4

import scala.annotation.tailrec
import scala.util.Random

case class Producer(id: Int, n: Int, maxWaiting: Int, container: Container) extends Thread {
  private val rng = Random(id)

  private def info(m: String): Unit = {
    println(s"Producer-$id: $m")
  }


  def produce: Unit = {
    val v = rng.nextInt(10)
    container.synchronized {
      while (container.isFull) {
        info("Container is full, waiting...")
        container.wait()
      }
      container.push(v)
      info(s"Value $v produced.")
      container.notifyAll()
    }
    Thread.sleep(rng.nextInt(maxWaiting))
  }


  override def run(): Unit = {
    (0 to n).foreach(_ => produce)
  }
}

object Producer {
  def builder(n:Int, maxWaiting:Int, container: Container): Int => Producer = id => Producer(id,n, maxWaiting, container)
}
