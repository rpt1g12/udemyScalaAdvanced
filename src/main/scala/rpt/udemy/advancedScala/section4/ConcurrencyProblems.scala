package rpt.udemy.advancedScala.section4

object ConcurrencyProblems {

  def runInParallel: Unit = {
    var x = 0

    val t0 = new Thread(()=>x=1)
    val t1 = new Thread(()=>x=2)

    t0.start()

    println(x)
  }

  case class BankAcc(var amount:Int)

  def buy(acc: BankAcc, thing:String, price:Int) : Unit =
    acc.amount -= price


  def atomicBuy(acc: BankAcc, thing:String, price:Int) : Unit = {
    acc.synchronized {
      acc.amount-=price // critical section subject to race conditions
    }
  }

  def demoBankAcc(n:Int) =
    (1 to n).foreach { _ =>
      val amount = 50000
      val shoePrice = 3000
      val phonePrice = 4000
      val acc = BankAcc(amount)
      val thread1 = new Thread(()=> buy(acc,"shoes",shoePrice))
      val thread2 = new Thread(()=> buy(acc,"phone",phonePrice))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if acc.amount != amount-shoePrice-phonePrice then println(s"I broke the bank system! I got ${acc.amount}$$ left")
    }

  def demoAtomicBankAcc(n:Int) =
    (1 to n).foreach { _ =>
      val amount = 50000
      val shoePrice = 3000
      val phonePrice = 4000
      val acc = BankAcc(amount)
      val thread1 = new Thread(()=> atomicBuy(acc,"shoes",shoePrice))
      val thread2 = new Thread(()=> atomicBuy(acc,"phone",phonePrice))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if acc.amount != amount-shoePrice-phonePrice then println(s"I broke the bank system! I got ${acc.amount}$$ left")
    }

  /**
   * Exercises
   * 1. Inception Threads:
   * thread1 -> thread2 -> ... threadN
   * each prints hello from thread i
   * print in reverse order.
   * 2. What is the min/max value of x in minMaxX func? min = 1 max = 100
   * 3. Whats the value of message? Awesome
   */

  def inception(n:Int):Unit = {
    def helper(i:Int=0):Unit = {
      if i<n then {
        val threadI = new Thread(() => helper(i+1))
        threadI.start()
        threadI.join()
      }
      println(s"Hello from thread $i")
    }
    helper()
  }

  def minMaxX = {
    var x = 0
    1 to 100 map(_ => new Thread(()=> x+=1)) foreach(_.start())
    println(x)
  }

  def demoSleepFallacy = {
    var message = ""
    val awesomeThread = new Thread(() => {
      Thread.sleep(1000)
      message = "Scala is awesome"
    })
    message = "Scala sucks"
    awesomeThread.start()
    Thread.sleep(1001)
    println(message)
  }


  def main(args: Array[String]): Unit = {
//    runInParallel
//
//    demoBankAcc(10000)
//
//    inception(100)
//
//    minMaxX

    demoSleepFallacy
  }

}
