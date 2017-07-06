package com.jamesmaggs.fpinscala

import scala.annotation.tailrec

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

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = (as, n) match {
    case (Nil, _) => Nil
    case (l, 0) => l
    case (Cons(_, t), i) => drop(t, i - 1)
  }

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => as
  }

  def append[A](as1: List[A], as2: List[A]): List[A] = foldRight(as1, as2)(Cons(_, _))

  def init[A](as: List[A]): List[A] =  as match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, a) => Cons(a, acc))

  def concatenate[A](ass: List[List[A]]): List[A] = foldLeft(ass, List[A]())(append)

  def increment(as: List[Int]): List[Int] = map(as)(_ + 1)

  def doublesToStrings(as: List[Double]): List[String] = map(as)(_.toString)

  def map[A,B](as: List[A])(f: A => B): List[B] = foldLeft(as, List[B]())((acc, a) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if(f(a)) List(a) else Nil)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())((a, acc) => append(f(a), acc))

  def add(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1, t2))
  }
}