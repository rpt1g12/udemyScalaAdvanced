package rpt.udemy.advancedScala.section6

object PathDependencyTypes extends App {

  class Outer {
    class Inner

    object InnerObject

    type InnerType

    def print(inner: Inner): Unit = println(inner)
    def printGeneral(inner: Outer#Inner): Unit = println(inner)
  }

  def aComplicatedMethod: Int = {
    class HelperClass
    type HelperType = String // Only type aliases
    return 42
  }

  val outer: Outer = new Outer
  val inner: outer.Inner = new outer.Inner

  val anotherOuter = new Outer // Different instances create different types.
  //  val otherInner: anotherOuter.Inner = new outer.Inner

  // Path dependent:
  outer.print(inner) // OK
  // But cant pass inner form another instance.
//  anotherOuter.print(inner)

  // All Inner classes inside Outer have common super type Outher#Inner
  anotherOuter.printGeneral(inner)

  /*
    Excersise:
      You are a developer of a small DB:
        * key by Int or String but what to be flexible to expand to other key types.
   */

  trait ItemLike {
    type Key
  }

  trait Item[K] extends ItemLike {
    override type Key = K
  }
  trait IntItem extends Item[Int] {
  }
  trait StringItem extends Item[String] {
  }

  /*
  This only works in Scala2

  // Make this work
  def get[T<:ItemLike](key:T#Key):T = ???

  get[IntItem](42)
  get[StringItem]("s")
//  get[StringItem](3)
  */

}
