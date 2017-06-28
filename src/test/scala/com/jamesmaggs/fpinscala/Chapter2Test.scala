package com.jamesmaggs.fpinscala

import com.jamesmaggs.fpinscala.Chapter2._
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalacheck.Prop._


object Chapter2Test extends Properties("Chapter 2") {

  private val smallInteger = choose(0, 500)

  property("fib(n) is the sum of the previous two") = forAll(smallInteger) { n =>
    val x = fib(n)
    val y = fib(n + 1)
    val z = fib(n + 2)
    x + y == z
  }

  private val compare = (a: Int, b:Int) => a <= b
  private val unsortedArray = containerOf[Array, Int](smallInteger) suchThat(a => !a.sameElements(a.sorted))

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
