package chapter4

import scala.util.control.NonFatal

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

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(value)  => Left(value)
    case Right(value) => Right(f(value))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e)  => Left(e)
    case Right(a) => f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_)  => b
    case Right(a) => Right(a)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b1 <- b
    yield f(a, b1)

object Either:
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match
      case Nil    => Right(Nil)
      case h :: t => (f(h).map2(traverse(t)(f)))(_ :: _)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

import Option.{Some, None}
import Either.{Left, Right}

@main def mainTest: Unit =
  println("Test")

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

def meanE(xs: Seq[Double]): Either[String, Double] =
  if xs.isEmpty then Left("mean of empty list")
  else Right(xs.sum / xs.length)

// Exercice 4.2
def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(mxs => mean(xs.map(x => math.pow(x - mxs, 2))))

// Exercie 4.3
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(xa => b.map(xb => f(xa, xb)))

// Exercice 4.4
def sequence[A](as: List[Option[A]]): Option[List[A]] =
  as match
    case Nil    => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
