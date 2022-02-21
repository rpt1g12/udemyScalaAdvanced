package rpt.udemy.advancedScala.section5

object Intro extends App {

  val pair = "a" -> 2

  case class Person(name:String) {
    def greet = println(s"Hi, my name is $name")
  }

  implicit def str2Person(s:String): Person = Person(s)

  "Peter".greet

}
