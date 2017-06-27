package com.jamesmaggs.fpinscala

import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {

    @tailrec
    def loop(count: Int, a: Int = 0, b: Int = 1): Int = count match {
      case 0 => a
      case _ => loop(count - 1, b, a + b)
    }

    loop(n)
  }

}
