package rpt.udemy.advancedScala.section4

import scala.annotation.tailrec
import scala.util.Random

case class Consumer(id: Int, n: Int, maxWaiting: Int, container: Container) extends Thread {
  private lazy val LOG = System.getLogger(this.getClass.getSimpleName)
  private val rng = Random(id)

  private def info(m: String): Unit = {
    println(s"Consumer-$id: $m")
  }

  private def consume: Unit = {
    container.synchronized {
      while (container.isEmpty) {
        info("Container is empty, waiting...")
        container.wait()
      }
      val v = container.pop
      info(s"Value $v consumed.")
      container.notifyAll()
    }
    Thread.sleep(rng.nextInt(maxWaiting))
  }


  override def run(): Unit = {
    (0 to n).foreach(_ => consume)
  }
}

object Consumer {
  def builder(n: Int, maxWaiting: Int, container: Container): Int => Consumer = id => Consumer(id, n, maxWaiting, container)
}
