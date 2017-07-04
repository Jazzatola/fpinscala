package com.jamesmaggs.fpinscala

import com.jamesmaggs.fpinscala.Generators._
import com.jamesmaggs.fpinscala.List._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

object Chapter3Test extends Properties("Chapter 3") {

  private def predicate(result: Boolean) = (i: Int) => result

  property("tail returns the elements without the head") = forAll(smallInteger, genList) { (i, l) =>
    tail(setHead(l, i)) == l
  }

  property("dropWhile removes everything if predicate is always true") = forAll(genList) { l =>
    dropWhile(l, predicate(true)) == Nil
  }

  property("dropWhile removes nothing if predicate is always false") = forAll(genList) { l =>
    dropWhile(l, predicate(false)) == l
  }

  property("dropWhile removes something at least") = forAll(genList, arbitrary[Int => Boolean]) { (l, f) =>
    length(l) >= length(dropWhile(l, f))
  }

  property("init and tail are the same for a single element") = forAll(smallInteger) { i =>
    val l = List(i)
    init(l) == tail(l)
  }

  property("length of empty list is zero") = {
    length(Nil) == 0
  }

  property("length of single element list is one") = forAll(smallInteger) { i =>
    length(List(i)) == 1
  }

  property("length of appended lists is the sum of both lengths") = forAll(genList, genList) { (l1, l2) =>
    length(append(l1, l2)) == length(l1) + length(l2)
  }

  property("length of the tail is one less than before") = forAll(genList suchThat(_ != Nil)) { l =>
    length(tail(l)) == length(l) - 1
  }

}

