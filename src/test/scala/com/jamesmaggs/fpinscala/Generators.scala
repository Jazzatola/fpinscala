package com.jamesmaggs.fpinscala

import org.scalacheck.Gen._
import org.scalacheck._

object Generators {

  val smallInteger: Gen[Int] = choose(0, 500)
  val unsortedArray: Gen[Array[Int]] = containerOf[Array, Int](smallInteger) suchThat(a => !a.sameElements(a.sorted))

  private val genEmpty = const(Nil)

  private val genCons = for {
    head <- smallInteger
    tail <- genList
  } yield Cons(head, tail)

  def genList: Gen[List[Int]] = oneOf(genEmpty, genCons)
}
