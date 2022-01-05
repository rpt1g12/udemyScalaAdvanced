package rpt.udemy.advancedScala.section4

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object ProducerConsumer extends App {

  private def runProducerConsumer(container:Container, nProd:Int, nCons:Int, producerBuilder: Int => Producer, consumerBuilder: Int => Consumer): Unit = {
    val producers = (1 to nProd).map(producerBuilder)
    val consumers = (1 to nCons).map(consumerBuilder)
    producers.foreach(_.start())
    consumers.foreach(_.start())
  }

  val container = Container(3)

  runProducerConsumer(
    container = container,
    nProd = 5, nCons = 5,
    producerBuilder = Producer.builder(10, 25, container),
    consumerBuilder = Consumer.builder(10, 50, container)
  )

}
