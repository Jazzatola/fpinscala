package com.jamesmaggs.fpinscala

import com.jamesmaggs.fpinscala.Chapter2._
import org.scalacheck._
import org.scalacheck.Prop._


object Chapter2Test extends Properties("Chapter 2") {

  private val smallInteger = Gen.choose(0, 500)

  property("fib(n) is the sum of the previous two") = forAll(smallInteger) { n =>
    val x = fib(n)
    val y = fib(n + 1)
    val z = fib(n + 2)
    x + y == z
  }
}
