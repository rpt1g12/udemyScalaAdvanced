package rpt.udemy.advancedScala.section2

import scala.util.Try

object AdvancedPatternMatching extends App {

  val numbers = List(1)

  val description = numbers match {
    case head :: Nil => "Only head available"
    case _ => "A normal list with Head and Tail"
  }

  println(description)

  // Matching options:
  /*
    - constants
    - wildcards
    - case classes
    - tuple
    - some special classes like prepend operation below. (that have unapply defined in companion object)
  */

  class Person(val name: String, val age: Int)

  object Person {
    def apply(name: String, age: Int) = new Person(name, age)

    def unapply(person: Person): Option[(String, Int)] = {
      if person.age < 41 then Some(person.name, person.age) else None
    }

    // Can be overloaded.
    def unapply(age: Int): Option[String] = {
      Some {
        if age < 18 then "Underaged" else "Fine"
      }
    }
  }

  val bob = new Person("Bob", 32)

  val bobsGreeting = bob match {
    case Person(name, age) => s"Hi, my name is $name and I am $age years old"
  }
  println(bobsGreeting)

  // This will fail with Match Error and wont get printed out
  val petersGreeting = Try {
    Person("Peter", 72) match {
      case Person(name, age) => s"Hi, my name is $name and I am $age years old"
    }
  }
  petersGreeting.foreach(println)

  // Use overloaded unapply method
  println {
    bob.age match {
      case Person(status) => s"I am $status"
    }
  }

  /*
    Excercise match type of number: Even, single digit, etc.
  */

  // Option 1
  {
    object singleDigit {
      def unapply(single: Int): Option[String] = if single < 10 then Some(s"$single is a Single Digit") else None
    }
    object even {
      def unapply(single: Int): Option[String] = if single % 2 == 0 then Some(s"$single is an Even number") else None
    }
    object odd {
      def unapply(single: Int): Option[String] = if single % 2 != 0 then Some(s"$single is a Odd number") else None
    }

    // This is single
    val numberType1: String = 1 match {
      case singleDigit(s) => s
      case even(e) => e
      case odd(o) => o
      case _ => "Something Else"
    }

    println(numberType1)

    // This is even
    val numberType234: String = 234 match {
      case singleDigit(s) => s
      case even(e) => e
      case odd(o) => o
      case _ => "Something Else"
    }

    println(numberType234)
  }


  // Option 2 Cleaner
  {
    object singleDigit {
      def unapply(single: Int): Boolean = single < 10
    }
    object even {
      def unapply(single: Int): Boolean = single % 2 == 0
    }
    object odd {
      def unapply(single: Int): Boolean = single % 2 != 0
    }

    // This is single
    val numberType1: String = 1 match {
      case singleDigit() => "Single Digit"
      case even() => "Even"
      case odd() => "Odd"
      case _ => "Something Else"
    }

    println(numberType1)

    // This is odd
    val numberType235: String = 235 match {
      case singleDigit() => "Single Digit"
      case even() => "Even"
      case odd() => "Odd"
      case _ => "Something Else"
    }

    println(numberType235)

  }

  /*
   ==== Part 2 ====
   */

  // 2.1 Infix Patterns (only works with 2 field case classes)
  case class Or[A, B](a: A, b: B)

  val either = Or(1, "one")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }

  // 2.2 Decomposing seqs
  val varargs = numbers match {
    case List(1, _*) => "Starts with a One"
    case init :+ 1 => "Ends with One"
  }

  trait MyList[+A] {
    def head: A

    def tail: MyList[A]
  }

  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  case object Empty extends MyList[Nothing] {
    override def head: Nothing = throw new RuntimeException("Empty list!")

    override def tail: MyList[Nothing] = this
  }

  object MyList {
    def unapplySeq[A](myList: MyList[A]): Option[Seq[A]] = myList match {
      case c: Cons[A] => unapplySeq(c.tail).map(tail => c.head +: tail)
      case _ => Some(Seq.empty)
    }
  }

  val myListInst: MyList[Int] = Cons(1, Cons(2, Empty))

  println {
    myListInst match {
      case MyList(1) => "Starts with 1 and ends with it"
      case MyList(1, 2, _*) => "Starts with 1 then 2 then something else"
      case MyList(1, _*) => "Starts with 1 then something else"
    }
  }

  // 2.3 Custom return types for unapply
  // isEmpty: Boolean, get: Something

  abstract class Wrapper[T] {
    def isEmpty: Boolean

    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false

      override def get: String = person.name
    }
  }

  println {
    Person("Sandorsan", 1000) match {
      case PersonWrapper(name) => s"This person name is $name"
    }
  }
}
