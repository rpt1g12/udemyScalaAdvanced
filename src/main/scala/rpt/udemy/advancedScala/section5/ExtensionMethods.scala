package rpt.udemy.advancedScala.section5

object ExtensionMethods extends App {

  // Scala 3 version of implicit classes are Extension
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  extension (string:String) {
    def greetAsPerson():String = Person(string).greet
  }

  println {
    "Rafa".greetAsPerson()
  }


  // Scala 2 Style
  object ExtensionMethods {
      implicit class MyRichInt(value: Int) {
        def isEven: Boolean = value % 2 == 0
        def sqrt: Double = Math.sqrt(value)
        def times[T](f:()=>T):Seq[T] = (1 to value).map(_ => f.apply())
        def *[T](seq:Seq[T]):Seq[T] = times(()=>seq).reduce(_++_)
      }
  }

  extension (value:Int) {
        def isEven: Boolean = value % 2 == 0
        def sqrt: Double = Math.sqrt(value)
        def times[T](f:()=>T):Seq[T] = (1 to value).map(_ => f.apply())
        def *[T](seq:Seq[T]):Seq[T] = times(()=>seq).reduce(_++_)
  }

  println {
    3.isEven
  }

  // Generic Extensions are allowed so we can extend List[A], Set[A], Dataset[A], etc...!!
  extension [A](list:List[A]) {
    def ends:(A,A) = (list.head,list.last)
    def extremes(using ordering:Ordering[A]):(A,A) = list.sorted.ends
  }

}
