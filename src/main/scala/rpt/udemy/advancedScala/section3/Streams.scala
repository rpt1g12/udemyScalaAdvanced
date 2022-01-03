package rpt.udemy.advancedScala.section3

import scala.util.Try

object Streams extends App {

  // All Integers >=0
  val s = MyStream.from(0)(_ + 1)
  // First take all evens then expand to even and next (odd)
  // Should return 0 to 10
  println {
    s.filter(_ % 2 == 0).flatMap(
      x => MyStream.from(x)(_ + 1).take(2)
    ).takeAsList(10)
  }

  println {
    (-1 #:: s).head
  }

  println {
    s.filter(_ < 10).take(10).toList
  }

  // Fails because there are not as many items in the filtered stream and keeps trying to find the third item.
  // s.filter(_ < 2).takeAsList(3)

  def fibonacci(f0: BigInt = 1, f1: => BigInt = 1): MyStream[BigInt] = {
    new LazyCons[BigInt](f0, fibonacci(f1, f0 + f1))
  }

  println {
    fibonacci().takeAsList(1000)
  }

  def eratosthenes(naturals: => MyStream[BigInt]=MyStream.from(BigInt(0))(_+1)): MyStream[BigInt] = {
    val h = naturals.head
    if (h < 2) {
      eratosthenes(MyStream.from(BigInt(2))(_ + 1))
    } else {
      new LazyCons[BigInt](h, eratosthenes(naturals.filter(_ % h != 0)))
    }
  }

  println {
    eratosthenes().take(100).toList
  }
}
