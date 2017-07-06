package com.jamesmaggs.fpinscala.chapter2

import scala.annotation.tailrec

object GettingStarted {

  def fib(n: Int): Int = {

    @tailrec
    def loop(count: Int, a: Int = 0, b: Int = 1): Int = count match {
      case 0 => a
      case _ => loop(count - 1, b, a + b)
    }

    loop(n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    }

    loop(1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
