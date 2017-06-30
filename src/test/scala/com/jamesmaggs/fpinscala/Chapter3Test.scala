package com.jamesmaggs.fpinscala

import com.jamesmaggs.fpinscala.Generators._
import com.jamesmaggs.fpinscala.List._
import org.scalacheck.Prop._
import org.scalacheck._

object Chapter3Test extends Properties("Chapter 3") {

  property("tail returns the elements without the head") = forAll(smallInteger, genList) { (i, l) =>
    tail(setHead(l, i)) == l
  }

  private def predicate(result: Boolean) = (i: Int) => result

  property("dropWhile removes everything if predicate is always true") = forAll(genList) { l =>
    dropWhile(l, predicate(true)) == Nil
  }

  property("dropWhile removes nothing if predicate is always false") = forAll(genList) { l =>
    dropWhile(l, predicate(false)) == l
  }
}

