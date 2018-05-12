package training

object Chapter2 {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def curry3[A, B, C, D](f: (A, B, C) => D): A => B => C => D = {
    (a: A) => (b: B) => (c: C) => f(a, b, c)
  }

}
