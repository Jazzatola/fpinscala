package com.jamesmaggs.fpinscala

object Chapter3 {
}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](as: List[A], a: A): List[A] = Cons(a, as)
}