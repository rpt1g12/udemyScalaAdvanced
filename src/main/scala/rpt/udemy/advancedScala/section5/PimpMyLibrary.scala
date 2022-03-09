package rpt.udemy.advancedScala.section5

import scala.util.Random

object PimpMyLibrary extends App {

  implicit class MyRichInt(value: Int) {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
    def times[T](f:()=>T):Seq[T] = (1 to value).map(_ => f.apply())
    def *[T](seq:Seq[T]):Seq[T] = times(()=>seq).reduce(_++_)
  }

  println {
    2.isEven
  }

  println {
    4.sqrt
  }

  // to is a method in RichInt 1 to 10

  import scala.concurrent.duration.* // seconds is a method in DurationConversions trait. 1.seconds


  /*
    Exercises
    1. Enrich String:
      - asInt
      - encrypt (with caesar cypher)
    2. Enrich Int:
      - execute value times a function ()=>T
      - Concat value times a Seq[T]
  */

  implicit class MyRichStr(value: String) {
    def asInt: Int = Integer.valueOf(value)
    def cypher(shift: Int): String = value.map(c => (c + shift).toChar)
  }

  println{
    "101".asInt
  }

  println{
    "abc".cypher(3) // def
  }

  println {
    val rgn = Random
    5.times(()=>rgn.nextInt()) // 5 random integers
  }

  println {
    3 * List(1,2,3) // List(1,2,3,1,2,3,1,2,3)
  }

}
