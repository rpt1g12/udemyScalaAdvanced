package rpt.udemy.advancedScala.section6

object FBoundedPolymorphism extends App {

  /*
  trait Animal {
    def breed: List[Animal]
  }

  // Nobody is forcing us to use the correct type in the list
  class Cat extends Animal {
    def breed: List[Cat]
  }

  class Dog extends Animal {
    def breed: List[Dog]
  }
  */

  // Option 1 - F-Bounded Polymorphism
  trait Animal[A] {
    def breed: List[Animal[A]]
  }

  // Now we need to use the correct type
  class Cat extends Animal[Cat] {
    override def breed: List[Animal[Cat]] = ???
  }

  class Dog extends Animal[Dog] {
    override def breed: List[Animal[Dog]] = ???
  }

  // But this compiles OK and is wrong
  class Turtle extends Animal[Dog] {
    override def breed: List[Animal[Dog]] = ???
  }

  // Option 2
  trait AnimalSelf[A<:AnimalSelf[A]] { self: A => // We enforce the type in paren to be the same as the class
    def breed: List[Animal[A]]
  }

  // Nobody is forcing us to use the correct type in the list
  class CatSelf extends AnimalSelf[CatSelf] {
    override def breed: List[Animal[CatSelf]] = ???
  }

  class DogSelf extends Animal[Dog] {
    override def breed: List[Animal[Dog]] = ???
  }

  // But..
  trait Fish extends Animal[Fish]
  class Shark extends Fish {
    override def breed: List[Animal[Fish]] = List(new Cod) // I can return a list of cods...
  }
  class Cod extends Fish {
    override def breed: List[Animal[Fish]] = ???
  }

  // Type Classes maybe??
  trait AnimalType
  trait CanBread[A] {
    def breed(animal:A):List[A]
  }

  class DogType extends AnimalType
  object DogType {
    implicit object DogsBread extends CanBread[DogType] {
      override def breed(animal: DogType): List[DogType] = List(new DogType)
    }
  }

}
