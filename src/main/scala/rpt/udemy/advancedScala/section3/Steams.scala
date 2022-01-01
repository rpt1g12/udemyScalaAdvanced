package rpt.udemy.advancedScala.section3

object Steams extends App{

  val s = MyStream.from(0)(_+1)
  s.filter(_%2==0).flatMap(x=>MyStream.from(x)(_+1).take(3)).take(10).forEach(println)
}
