package rpt.udemy.advancedScala.section6

object RockingInheritance extends App {

  // Convenience
  trait Writer[T] {
    def write(value:T):Unit
  }
  trait Closable {
    def close(status:Int):Unit
  }
  trait GenericStream[T] {
    def foreach(f: T=>Unit):Unit
  }

  def processStream[T](steam:GenericStream[T] with Writer[T] with Closable): Unit = {
    steam.foreach(println)
    steam.close(0)
  }

  // Diamond problem
  trait Animal {
    def name:String
  }
  trait Lion extends Animal {
    override def name: String = "Lion"
  }
  trait Tiger extends Animal {
    override def name: String = "Tiger"
  }
  class Surprise extends Lion with Tiger
  class Liger extends Lion with Tiger {
    override def name: String = "Weird cat.."
  }

  println {
    val ? = new Surprise
    ?.name // Prints Tiger (LAST OVERWITE PREVAILS)
  }

  // The super problem. Type Linearisation

  trait Cold {
    def print:Unit = println("cold")
  }
  trait Green extends Cold {
    override def print: Unit = {
      println("green")
      super.print
    }
  }
  trait Blue extends Cold {
    override def print: Unit = {
      println("blue")
      super.print
    }
  }
  class Red {
    def print:Unit = println("red")
  }

  class White extends Red with Green with Blue {
    override def print: Unit = {
      println("white")
      super.print
    }
  }

  {
    val white = new White
    white.print
  }


}
