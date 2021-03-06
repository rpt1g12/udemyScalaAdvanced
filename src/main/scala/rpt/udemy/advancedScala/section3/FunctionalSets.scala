package rpt.udemy.advancedScala.section3

import scala.util.Random

object FunctionalSets extends App {

  val s = MySet(1,2,3,4)
  // 1,2,3,4,5
  println {
    s + 5
  }
  // 1,2,3,4,5,6,7
  println {
    s ++ MySet(5,6,7)
  }
  // 10,100,20,200,30,300,40,400
  println {
    s map {
      e => e*10
    } flatMap {
      e => MySet(e, e * 10)
    }
  }
  // 1,3
  println {
    s"Filter: ${s filter (_%2==1)}"
  }

  // 4,3,1
  println {
    s"Substract: ${s - 2}"
  }

  // 1,4
  println {
    s"Difference: ${s -- MySet(3,2)}"
  }

  // 4, 3,2
  println {
    s"Intersection: ${s & MySet(3,2,4,5)}"
  }

  // 1,5
  println {
    val A = s
    val B = MySet(3,2,4,5)
    s"(A++B)--(A&B): ${ (A++B) -- (A&B) }"
  }

  // true
  println {
    s"!s(5): ${(!s)(5)}"
  }

  // All Integers
  val INTS = s ++ (!s)
  // Should print always true
  println {
    val rgn = Random(System.nanoTime())
    (0 to 1000).map {
      x=>INTS(rgn.nextInt)
    }.reduce(_ && _)
  }

  println {
    s"!s(5): ${(!s)(5)}" + "\n" +
    s"!s.filter(_%2)(5): ${(!s).filter(_%2==0)(5)}"
  }
}
