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

  def tail[A](as: List[A]): List[A] = drop(as, 1)

  def setHead[A](as: List[A], a: A): List[A] = Cons(a, as)

  def drop[A](as: List[A], n: Int): List[A] = (as, n) match {
    case (Nil, _) => Nil
    case (l, 0) => l
    case (Cons(_, tail), i) => drop(tail, i - 1)
  }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => if(f(head)) dropWhile(tail, f) else as
  }

}