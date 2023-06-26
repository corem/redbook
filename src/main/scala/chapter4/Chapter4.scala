package chapter4

enum Option[+A]:
    case Some(get: A)
    case None

import Option.{Some, None}
@main def mainTest: Unit =
    println("Test")

def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)