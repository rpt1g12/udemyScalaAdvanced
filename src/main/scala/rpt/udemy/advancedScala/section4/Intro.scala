package rpt.udemy.advancedScala.section4

import java.util.concurrent.Executors

object Intro extends App {

  /**
   * JVM Threads
   */
  val aThread = new Thread(() => println("Running in parallel"))

  // Creates a JVM thread that runs on top of OS thread
  aThread.start() // Gives signal to JVM to start a JVM thread
  aThread.join() // Blocks until it finishes running

  val threadHello = new Thread(() => 1 to 5 foreach(x => println("Hello")))
  val threadGoodBye = new Thread(() => 1 to 5 foreach(x => println("Bye")))

  // Order is different in each execution
  threadHello.start()
  threadGoodBye.start()

  /**
   * Executors:
   *
   * Threads are very expensive to create and kill. Executors come in rescue.
   */
  val pool = Executors.newFixedThreadPool(2)
  pool.execute(() => println("Something in the thread pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println(s"Done after 1s @${System.currentTimeMillis()}")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println(s"Half done in 1s @${System.currentTimeMillis()}")
    Thread.sleep(1000)
    println(s"Done after 2s @${System.currentTimeMillis()}")
  })

//  pool.shutdown()
//  pool.execute(() => println("This will fail because pool is shut down but wont interrupt previous executes"))
//  pool.shutdownNow() // Will throw 2 exceptions for as it will interrupt previous executes while sleeping
//  println(pool.isShutdown) // Will print true even if previous executes havent finished


}
