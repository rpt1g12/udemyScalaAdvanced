package rpt.udemy.advancedScala.section1

import scala.util.Try

object DarkSugars extends App{

  // 1. Methods with single parenthesis
  def singleArgMethod(arg: Int): String = arg.toString

  val argsInBlock = singleArgMethod {
    // Some code
    34
  }

  // Tipical in try blocks like in Java
  val aTryInstance = Try {
    throw new RuntimeException("Hell no!")
  }

  // For map, flatmap, etc.
  val aList = List(1,2,3)
  val mappedList = aList map {
    _+1
  }

  // 2. Single abstract method.
  trait Action {
    def perform(x: Int): Int
  }

  val anActionInstance: Action = x => x +1

  // Example: Runnable
  val aThread = new Thread( new Runnable {
    override def run(): Unit = println("hello Scala")
  })

  val aSweeterThread = new Thread(() => println("Hello sweeter Scala"))

  // Example Abstract classes
  abstract class AbstractType {
    val implemented: Int = 8
    def f(x:Int): Unit
  }

  val abstractInstance: AbstractType = anInt => println("Hello abstract sweet scala")

  // 3. :: and #:: methods are special because last char of method is a : which makes it left associative
  val prepended = 2 :: 3 :: Nil // List(2,3)

  class MyStream[T] {
    def -->: (t: T): MyStream[T] = this
  }

  val magicAssociation = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // 4. Multi-word method naming
  class TeenGirl(name: String) {
    def `and then said` (something:String): String = s"And then $name said: $something."
  }
  println(new TeenGirl("Maria") `and then said` "sweeet sintax") // And then Maria said: sweet sintax

  // 5. Infix types
  class Composite[A,B]
  val composite: Int Composite String = new Composite[Int, String]

  class ->[A,B]
  val arrow: Int -> String = new ->[Int, String]

  // 6. Update method (similar to apply) [MUTABLE CONTAINERS]
  val anArray = Array(1,2,3)
  anArray(2) = 7 // acutally is calling $ anArray.update(2,7)

  // 7. Setters in Mutable Containers
  class Mutable {
    private var privateVar: Int = 0 // private for OO encapsulation
    def member: Int = privateVar // Getter
    def member_=(memberVar:Int): Unit = privateVar = memberVar // Setter
  }

  val mutableExample = new Mutable
  mutableExample.member = 3
  println(mutableExample.member)

}
