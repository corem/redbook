package chapter4

enum Option[+A]:
  case Some(get: A)
  case None

  // Exercice 4.1
  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None    => None

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case Some(a) => a
    case None    => default

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match
    case Some(a) if f(a) => this
    case _               => None

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    a.flatMap(xa => b.map(xb => f(xa, xb)))

import Option.{Some, None}
@main def mainTest: Unit =
  println("Test")

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercice 4.2
def variance(xs: Seq[Double]): Option[Double] = 
  mean(xs).flatMap(mxs => mean(xs.map(x => math.pow(x - mxs, 2))))
