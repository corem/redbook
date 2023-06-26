package chapter2

object Chapter2:

  // Exercise 2.1
  private def fibonacci(n: Int):Int =
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if n <= 0 then current
      else go(n - 1, next, current + next)
    go(n, 0, 1)

  private def formatFibonacci(n: Int): String =
    val msg = "fibonacci(%d) = %d"
    msg.format(n, fibonacci(n))

  // Exercice 2.2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean =
    @annotation.tailrec
    def go(n: Int): Boolean =
      if n >= as.length-1 then true
      else if gt(as(n), as(n+1)) then false
      else go(n+1)
    go(0)

  // Exercice 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => (b => f(a,b))

  // Exercice 2.4
  def uncurry[A,B,C](f: A => (B => C)): (A,B) => C =
    (a, b) => f(a)(b)

  // Exercice 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def aToC = compose[Int, Int, Int]((b: Int) => 2 * b, (a: Int) => 3 * a)

  @main def mainTest: Unit =
    println(formatFibonacci(8))
    println(isSorted[Int](Array(1,3,2,6,4), _ > _))
    println(isSorted[Int](Array(1,2,3,4,5), _ > _))
    println(aToC(2))



