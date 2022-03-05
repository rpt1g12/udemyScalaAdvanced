package rpt.udemy.advancedScala.section5

object OrganisingImplicits extends App {

  case class Person(name: String, age: Int)

  val people = List(
    Person("Anabel", 40) ,
    Person("Jane", 25) ,
    Person("Marcus", 52) ,
    Person("Marcus", 32) ,
    Person("Tutankamon", 3000)
  )

  implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan(_.age > _.age)

  object OrderByInt {
    implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan(_.age <= _.age)
  }
  object OrderByString {
    implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan {
      case (Person(nameA,_), Person(nameB,_)) => nameA.compareTo(nameB) <= 0
    }
  }

  /**
   * Implicit scope
   * 1. Normal Scope (Local Scope)
   * 2. Imported Scope
   * 3. Companion Objects of all types involved
   * will work if implicit is defined in the Person's companion but not in other ones
   */

  // Prints in reverse order of age
  people.sorted.foreach(println)
  import OrderByString._
  // Prints in alphabetical order
  people.sorted.foreach(println)
  import OrderByInt._
  // Prints in ascending order of age
  people.sorted.foreach(println)
}
