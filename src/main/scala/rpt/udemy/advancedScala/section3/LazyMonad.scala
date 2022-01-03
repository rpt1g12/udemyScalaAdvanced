package rpt.udemy.advancedScala.section3

import rpt.udemy.advancedScala.section3.LazyMonad.flatten

import scala.annotation.tailrec

class LazyMonad[+A](v: => A) {
  lazy private val internalVal = v

  def get: A = internalVal

  def map[B](foo: (=> A) => B): LazyMonad[B] = flatMap(x => LazyMonad(foo(x)))

  def flatMap[B](foo: (=> A) => LazyMonad[B]): LazyMonad[B] = {
    foo(internalVal)
  }
}

object LazyMonad {
  def apply[A](v: => A): LazyMonad[A] = new LazyMonad[A](v)

  def flatten[A](m: => LazyMonad[LazyMonad[A]]): LazyMonad[A] = m.flatMap(x => x)
}


