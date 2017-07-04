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
    case (Cons(_, t), i) => drop(t, i - 1)
  }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if(f(h)) dropWhile(t, f) else as
  }

  def append[A](as1: List[A], as2: List[A]): List[A] = as1 match {
    case Nil => as2
    case Cons(h,t) => Cons(h, append(t, as2))
  }

  def init[A](as: List[A]): List[A] =  as match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)((acc, i) => acc + i)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)((acc, d) => acc * d)

  def length[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, a) => Cons(a, acc))

}