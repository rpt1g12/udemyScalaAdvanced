package rpt.udemy.advancedScala.section5

object EqualizerPlayground extends App {

  implicit class EqualizerEnrichment[T](value: T) {
    def ===(other:T)(implicit equalizer: Equalizer[T]):Boolean = {
      Equalizer(value,other)
    }
    def =!=(other:T)(implicit equalizer: Equalizer[T]):Boolean = ===(other)
  }

  val john = User("John", 30, "j@email.com")
  val anotherJohn = john.copy(email = "jt@system.cat")

  println {
    john === anotherJohn // prints false
  }

  {
    implicit val nameEq: Equalizer[User] = User.Equalizers.nameEqualizer
    println {
      john === anotherJohn // prints true
    }
  }

}
