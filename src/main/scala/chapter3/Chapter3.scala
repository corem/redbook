package chapter3

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  // Section 3.2
  def sum(ints: List[Int]): Int = ints match
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  // Exercice 3.1
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  // Exercice 3.2
  def tail[A](as: List[A]): List[A] = as match
    case Nil         => sys.error("tail of empty list")
    case Cons(_, tl) => tl

  // Exercice 3.3
  def setHead[A](as: List[A], h: A): List[A] = as match
    case Nil         => sys.error("setHead of empty list")
    case Cons(_, xs) => Cons(h, xs)

  // Exercice 3.4
  def drop[A](as: List[A], n: Int): List[A] =
    if n <= 0 then as
    else
      as match
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)

  // Exercice 3.5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => as

  // Exercice 3.6
  def init[A](as: List[A]): List[A] = as match
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))

import List.*
@main def mainTest: Unit =
  val list1: List[Double] =
    List.Cons(2.0, List.Cons(3.5, List.Cons(4.3, List.Nil)))
  val list2: List[Int] = List.Cons(1, List.Cons(2, List.Nil))
  val list3: List[String] = List.Cons("a", List.Cons("b", List.Nil))

  println(list1)
  println(product(list1))
  println(list2)
  println(sum(list2))
  println(list3)
  println(tail(list2))
  println(setHead(list2, 3))
  println(drop(list1, 2))
