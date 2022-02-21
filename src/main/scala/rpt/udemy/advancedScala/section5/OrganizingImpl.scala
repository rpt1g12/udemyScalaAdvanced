package rpt.udemy.advancedScala.section5

object OrganizingImpl extends App {

  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan[Int](_>_)
  println(List(1,2,4,6,0).sorted)

  /*
    Implicits:
      1. val/var
      2. object
      3. parameterless def
  */

  case class Person(name:String, age:Int)
  object Person {
    implicit val alphabeticalOrder: Ordering[Person] = Ordering.fromLessThan[Person](_.name<_.name)
  }
  implicit val ageOrder: Ordering[Person] = Ordering.fromLessThan[Person](_.age<_.age)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 24),
    Person("John",60)
  )


  println(persons.sorted)

  /*
    Implicit Scope
    1. Normal Scope = Local
    2. imported scope
    3. Companion objects of all types in the method signature
  */

  case class Purchase(units:Int, price:Double){
    lazy val total: Double = units*price
  }
  object Purchase {
    implicit val totalPrice: Ordering[Purchase] = Ordering.fromLessThan[Purchase](_.total<_.total)
  }
  object PriceOrdering {
    implicit val ordering: Ordering[Purchase] = Ordering.fromLessThan[Purchase](_.price<_.price)
  }
  object UnitsOrdering {
    implicit val ordering: Ordering[Purchase] = Ordering.fromLessThan[Purchase](_.units<_.units)
  }

  /*
    1. Total price most used
  */

}
