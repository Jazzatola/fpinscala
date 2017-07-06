package com.jamesmaggs.fpinscala.chapter2

import com.jamesmaggs.fpinscala.chapter2.GettingStarted._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


object GettingStartedTest extends Properties("Getting Started") {

  private val compare = (a: Int, b:Int) => a <= b
  private val smallInteger: Gen[Int] = choose(0, 500)
  private val unsortedArray: Gen[Array[Int]] = containerOf[Array, Int](smallInteger) suchThat(a => !a.sameElements(a.sorted))

  property("fib(n) is the sum of the previous two") = forAll(smallInteger) { n =>
    val x = fib(n)
    val y = fib(n + 1)
    val z = fib(n + 2)
    x + y == z
  }

  property("isSorted is false for unsorted arrays") = forAll(unsortedArray) { ns =>
    !isSorted(ns, compare)
  }

  property("isSorted is true for empty array") = {
    isSorted(Array(), compare)
  }

  property("isSorted is true for single element array") = forAll(smallInteger) { n =>
    isSorted(Array(n), compare)
  }

  property("isSorted is true for all sorted arrays by induction") = forAll(unsortedArray) { ns =>
    val sortedArray = ns.sortWith(compare)
    val Array(first, second, rest @ _*) = sortedArray
    isSorted(sortedArray, compare) ==> (compare(first, second) && isSorted(second +: rest.toArray, compare))
  }
}
