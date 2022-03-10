package rpt.udemy.advancedScala.section5

import rpt.udemy.advancedScala.section5.TypeClasses.{HTMLSerializer, User}

import java.util.Date

object HTMLSerialization extends App {

  trait HTMLSerializer[T] {
    def serialize(x: T): String
  }
  object HTMLSerializer {
    def serialize[T](value:T)(implicit htmlSerializer: HTMLSerializer[T]): String = {
      htmlSerializer.serialize(value)
    }
    def apply[T](implicit serializer:HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }

  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = HTMLSerializer.serialize(value)
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(x: Int): String = s"<div style: color=blue>value</div>"
  }

  implicit object DateSerializer extends HTMLSerializer[java.util.Date] {
    override def serialize(x: Date): String = s"<div>${x.toString}</div>"
  }

  case class User(name: String, age: Int, email: String)

  object User {
    object HTMLSerialisers {
      val userSerializer: HTMLSerializer[User] = (x: User) => {
        s"<div>${x.name} ${x.age} <a href=${x.email}/></div>"
      }
      val partialUserSerializer: HTMLSerializer[User] = (x: User) => {
        s"<div>${x.name}</div>"
      }
    }
    implicit val defaultSerializer: HTMLSerializer[User] = HTMLSerialisers.userSerializer
  }

  val john = User("John", 30, "j@email.com")
  val anotherJohn = john.copy(email = "jt@system.cat")

  println{
    john.toHTML
  }
  println{
    john.toHTML(User.HTMLSerialisers.partialUserSerializer)
  }
}
