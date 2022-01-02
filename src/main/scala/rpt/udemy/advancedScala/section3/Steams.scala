package rpt.udemy.advancedScala.section3

object Steams extends App{

  // All Integers >=0
  val s = MyStream.from(0)(_+1)
  // First take all evens then expand to even and next (odd)
  // Should return 0 to 10
  println {
    s.filter(_%2==0).flatMap(
      x=>MyStream.from(x)(_+1).take(2)
    ).takeAsList(10)
  }
}
