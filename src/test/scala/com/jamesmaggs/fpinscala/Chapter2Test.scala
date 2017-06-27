package com.jamesmaggs.fpinscala

import com.jamesmaggs.fpinscala.Chapter2._

class Chapter2Test extends UnitTest {

  test("Fibonacci sequence") {
    val sequence = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
    val assertion = (value: Int, index: Int) => assert(fib(index) == value)
    sequence.zipWithIndex.foreach(assertion.tupled)
  }

}
