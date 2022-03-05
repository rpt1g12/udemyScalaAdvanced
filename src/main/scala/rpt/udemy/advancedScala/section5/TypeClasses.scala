package rpt.udemy.advancedScala.section5

import java.util.Date

object TypeClasses extends App {

  trait HTMLWrittable {
    def toHTML: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWrittable {
    override def toHTML: String = s"<div>$name $age <a href=$email/></div>"
  }


  val john = User("John", 30, "j@email.com")

  /*
    1. Only for the types WE write
    2. ONE implementation of MANY possible:
      Option 1: Pattern Matching
  */
  object HTMLSerializerPM {
    def serialize(x: Any): String = x match {
      case User(name, age, email) => s"<div>$name $age <a href=$email/></div>"
      case d: java.util.Date => s"<div>${d.toString}</div>"
      case _ => ""
    }
  }
  /*
   but:
    * type safety lost
    * need to modify cases for each new class
    * only ONE implementation (i.e. if user is logged in we change html)

  Better option
  */

  trait HTMLSerializer[T] {
    def serialize(x: T): String
  }

  implicit object UserSerializer extends HTMLSerializer[User] {
    override def serialize(x: User): String = s"<div>${x.name} ${x.age} <a href=${x.email}/></div>"
  }

  println {
    UserSerializer.serialize(john)
  }

  /*
    With this design pattern:
      1. We can define serializers for other types.
  */
  object DateSerializer extends HTMLSerializer[java.util.Date] {
    override def serialize(x: Date): String = s"<div>${x.toString}</div>"
  }

  //  2. We can define multiple serializers for same  type
  object PartialUserSerializer extends HTMLSerializer[User] {
    override def serialize(x: User): String = s"<div>${x.name}</div>"
  }

  /*
    TYPE CLASSES
  */
  trait Equality[T] {
    def test(left: T, right:T): Boolean
  }

  object Equality {
    def apply[T](left: T, right:T)(implicit equalizer: Equality[T]): Boolean = equalizer.test(left,right)
    def apply[T](implicit instance:Equality[T]): Equality[T] = instance
  }

  object UserNameEquality extends Equality[User]{
    override def test(left: User, right: User): Boolean = left.name.equals(right.name)
  }
  object UserNameEmailEquality extends Equality[User]{
    override def test(left: User, right: User): Boolean = left.name.equals(right.name) && left.email.equals(right.email)
  }

  // PART 2
  object HTMLSerializer {
    def serialize[T](value:T)(implicit htmlSerializer: HTMLSerializer[T]): String = {
      htmlSerializer.serialize(value)
    }

    /**
     * Surfaces an implicit HTMLSerializer of type T
     * @param serializer HTMLSerializer
     * @tparam T Type of the HTMLSerializer
     * @return Serializer in scope for type T
     */
    def apply[T](implicit serializer:HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(x: Int): String = s"<div style: color=blue>x</div>"
  }

  implicit class WithHTML(x:Any) {
    def toHTML(implicit serializer:HTMLSerializer[Any]):String = HTMLSerializer.serialize(x)
  }

  println(HTMLSerializer.serialize(john))
  println(HTMLSerializer[User].serialize(john))
  println(john.toHTML)

  // Exercise
  val anotherJohn = User("John", 33, "jt@system.cat")

  {
    implicit val equalizer: Equality[User] = UserNameEquality
    println(Equality(john, anotherJohn))
  }

  object User {
    implicit val equalizer: Equality[User] = UserNameEmailEquality
  }
  println(Equality[User].test(john, anotherJohn))
}
