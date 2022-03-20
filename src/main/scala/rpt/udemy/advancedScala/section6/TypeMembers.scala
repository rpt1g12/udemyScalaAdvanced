package rpt.udemy.advancedScala.section6

object TypeMembers {

  class Animal
  class Dog extends Animal
  class Cat extends Animal


  class AnimalCollection {
    type AnimalType // abstract type member
    type BoundedAnimal <: Animal // Upper bounded in Animal (must extend Animal)
    type SuperBoundedAnimal >: Dog <: Animal // Lower bounded in Dog, upper bounded in Animal.
    type AnimalCat = Cat // Type alias
  }

  val animalCollection: AnimalCollection = new AnimalCollection
  val dog: animalCollection.AnimalType = ???
//  val cat:animalCollection.SuperBoundedAnimal = new Cat // Not allowed because is lower bounded by Dog
  val pup: animalCollection.SuperBoundedAnimal = new Dog // OK.
  val cat: animalCollection.AnimalCat = new Cat

  type CatAlias = Cat
  val anotherCat: CatAlias = new Cat

  // We can use as an alternative to generics
  trait MyList {
    type T
    def add(element:T):MyList
  }

  class MyIntList(head:Int, tail:MyList) extends MyList {
    override type T = Int
    def add(element:Int):MyList = new MyIntList(element, this)
  }

  // .type
  type CatsType = cat.type
  val newCat: CatsType = cat


  /*
    Exercise:
      * You have the followig trait defined wich you cannot modify (LOCKED).
      * You thik MList should only be applicable to numbers.

  */
  trait MList {
    type T
    def head:T
    def tail:MList
  }

  trait ApplicableOnlyToNumbers extends MList {
    type T <: Number
  }

  // This should not compile
//  class MStrList(hd:String, tl:MStrList) extends ApplicableOnlyToNumbers {
//    type T = String
//    def head: String = hd
//    def tail:MStrList = tl
//  }

  // Only this should compile.
  class MIntList(hd:Integer, tl:MIntList) extends ApplicableOnlyToNumbers {
    type T = Integer
    def head: Integer = hd
    def tail:MIntList = tl
  }

}
