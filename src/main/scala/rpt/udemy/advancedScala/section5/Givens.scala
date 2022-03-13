package rpt.udemy.advancedScala.section5

object Givens extends App {

  case class Num(n: Int)

  val aList = List(Num(1), Num(2), Num(3), Num(4))
  val anOrderedList = aList.sorted // Implicit Ordering[Int]

  // Scala 2 style
  object Implicits {
    implicit val descendingOrdering: Ordering[Num] = Ordering.fromLessThan(_.n > _.n)
  }

  // Scala 3 style

  object Givens {
    given descendingOrdering: Ordering[Num] = Ordering.fromLessThan(_.n > _.n)
  }

  given descendingOrder: Ordering[Num] = (x: Num, y: Num) => y.n - x.n

  object GivenWith {
    given descendingOrder: Ordering[Num] with {
      override def compare(x: Num, y: Num): Int = x.n - y.n
    }
  }

  // Import whatever implicit/given you want

  import GivenWith.given // import *._ will not work in Scala3

  println(anOrderedList) // will print 4,3,2,1 even if given is defined after the ordered list
  println {
    aList.sorted // Should use GivenWith implicit ordering because it's used after importing given
  }

  // Implicit arguments:

  // Scala 2 style
  def extremes_s2[A](list: List[A])(implicit ordering: Ordering[A]): (A, A) = {
    val orderedList = list.sorted
    (orderedList.head, orderedList.last)
  }

  // Scala 3 style
  def extremes_s3[A](list: List[A])(using ordering: Ordering[A]): (A, A) = {
    val orderedList = list.sorted
    (orderedList.head, orderedList.last)
  }

  // Implicit defs
  trait Combinator[A] {
    def combine(left: A, right: A): A
  }

  // Scala 2 style
  implicit def listOrdering_s2[A](implicit simpleOrdering: Ordering[A], combinator: Combinator[A]): Ordering[List[A]] = new Ordering[List[A]] {
    override def compare(x: List[A], y: List[A]): Int = {
      val sumX = x.reduce(combinator.combine)
      val sumY = y.reduce(combinator.combine)
      simpleOrdering.compare(sumX, sumY)
    }
  }

  // Scala 3 style
  given listOrdering_s3[A] (using simpleOrdering: Ordering[A], combinator: Combinator[A]): Ordering[List[A]] with {
    override def compare(x: List[A], y: List[A]): Int = {
      val sumX = x.reduce(combinator.combine)
      val sumY = y.reduce(combinator.combine)
      simpleOrdering.compare(sumX, sumY)
    }
  }

  // Implicit conversions
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  // Scala 2
  implicit def string2Person(string: String): Person = Person(name = string)

  println {
    "Rafa".greet
  }

  // Scala 3

  import scala.language.implicitConversions // required in scala 3

  given string2PersonConversion: Conversion[String, Person] with {
    override def apply(x: String): Person = Person(x)
  }

  println {
    "Guille".greet
  }

}
