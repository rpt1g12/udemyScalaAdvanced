package rpt.udemy.advancedScala.section3

object Monads extends App {

  println {
    s"Attempt 1/0: ${
      Attempt {
        1 / 0
      }
    }"
  }

  println {
    s"Left property Unit.flatmap(x=>f(x)) = f(x):\n ${
      Attempt(3).flatMap(x=>Attempt(x*2+"s"))
    }"
  }

  /**
   * Exercices:
   *
   * 1. Lazy monad. Only evaluated when needed
   * 2. Monad with map + flatten
   */

  println {
    val foo: (=> Int) => LazyMonad[Int] = LazyMonad(_)
    LazyMonad(1/0).flatMap(foo)
  }

  val lazyV = LazyMonad {
    println("Evaluated")
    8
  }

  val lazyFlatMapV = lazyV.flatMap(x=> LazyMonad(x*2))
  val lazyFlatMapV2 = lazyV.flatMap(x=> LazyMonad(x*3))

  lazyFlatMapV.get
  lazyFlatMapV2.get

}
