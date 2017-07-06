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

  property("sum of empty list is zero") = {
    sum(Nil) == 0
  }

  property("sum of a list") = forAll(smallInteger, genList) { (i, l) =>
    sum(setHead(l, i)) == sum(l) + i
  }

  property("product of empty list is one") = {
    product(Nil) == 1.0
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

  property("reverse a list twice to get back to the original") = forAll(genList) { l =>
    reverse(reverse(l)) == l
  }

  property("reverse doesn't change the length") = forAll(genList) { l =>
    length(reverse(l)) == length(l)
  }

  property("concatenate some lists") = forAll(genList, genList, genList) { (l1, l2, l3) =>
    concatenate(List(l1, l2, l3)) == append(l1, append(l2, l3))
  }

  property("length of concatenated lists is the sum of all lengths") = forAll(genList, genList, genList) { (l1, l2, l3) =>
    length(concatenate(List(l1, l2, l3))) == length(l1) + length(l2) + length(l3)
  }

  property("increment a list") = forAll(genList) { l =>
    sum(increment(l)) == sum(l) + length(l)
  }

  property("filter removes something at least") = forAll(genList, arbitrary[Int => Boolean]) { (l, f) =>
    length(l) >= length(filter(l)(f))
  }

  property("filter out some odd numbers") = {
    filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 0) == List(2, 4, 6, 8)
  }

  property("flatMap example") = {
    flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3)
  }

  property("flatMap - left identity") = forAll(smallInteger) { i =>
    val f = (i: Int) => List(i ^ 2)
    flatMap(List(i))(f) == f(i)
  }

  property("flatMap - right identity") = forAll(genList) { l =>
    flatMap(l)(List(_)) == l
  }

  property("flatMap - associativity") = forAll(genList) { l =>
    val f = (i: Int) => List(i ^ 2)
    val g = (i: Int) => List(i + 3)
    flatMap(flatMap(l)(f))(g) == flatMap(l)(i => flatMap(f(i))(g))
  }

  property("add two lists together") = {
    add(List(1,2,3), List(4,5,6)) == List(5,7,9)
  }

  property("the init of the tail is a subsequence") = forAll(genList suchThat(_ != Nil)) { l =>
    hasSubsequence(l, init(tail(l)))
  }

}

