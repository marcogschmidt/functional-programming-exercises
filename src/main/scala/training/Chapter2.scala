package training

object Chapter2 extends App {

  /**
    * Exercise 2.1
    * Return n-th Fibonacci number.
    */
  def fib(n: Int): Int = {

    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) -1
      else if (n == 1) prev
      else loop(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  /**
    * Exercise 2.2
    * Check whether array is sorted according to comparison function.
    * Example of checkOrder: (cur: Int, next: Int) => cur <= next
    */
  def isSorted[A](as: Array[A], checkOrder: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (checkOrder(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  /**
    * Exercise 2.3
    * Convert function f of one argument into a function of one argument that partially applies f.
    */
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => b: B => f(a, b)
  }

  /**
    * Exercise 2.4
    * Reverse then transformation applied by the above function.
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B)=> f(a)(b)
  }

  /**
    * Exercise 2.5
    * Composes two functions.
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  /**
    * Try currying a function with three arguments.
    */
  def curry3[A, B, C, D](f: (A, B, C) => D): A => B => C => D = {
    a: A => (b: B) => (c: C) => f(a, b, c)
  }

}
