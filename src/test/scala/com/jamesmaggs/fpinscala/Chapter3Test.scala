package com.jamesmaggs.fpinscala

import com.jamesmaggs.fpinscala.Generators._
import com.jamesmaggs.fpinscala.List.{setHead, tail}
import org.scalacheck.Prop._
import org.scalacheck._

object Chapter3Test extends Properties("Chapter 3") {

  property("tail returns the elements without the head") = forAll(smallInteger, genList) { (i, l) =>
    tail(setHead(l, i)) == l
  }

}

