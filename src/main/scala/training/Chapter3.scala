package training

object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Const[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) {
        Nil
      } else {
        Const(as.head, apply(as.tail: _*))
      }
    }

    /**
      * Exercise 3.2
      * Remove first element from list.
      */
    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Const(_, xs) => xs
    }

    /**
      * Exercise 3.3
      * Replace first element of a list with a value.
      */
    def setHead[A](as: List[A], head: A): List[A] = as match {
      case Nil => Const(head, Nil)
      case Const(_, xs) => Const(head, xs)
    }


    /**
      * Exercise 3.4
      * Remove first n elements from a list.
      */
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Const(_, xs) => if (n > 0) {
        drop(xs, n - 1)
      } else {
        l
      }
    }

    /**
      * Exercise 3.5
      * Removes elements from the list prefix as long as they match a predicate.
      */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Const(a, as) if f(a) => dropWhile(as, f)
      case _ => l
    }

    /**
      * Curried version of 'dropWhile' for better type inference.
      */
    def betterDropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Const(a, as) if f(a) => betterDropWhile(as)(f)
      case _ => l
    }

    /**
      * Exercise 3.6
      * Returns a list with all but the last element af a list.
      */
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Const(_, Nil) => Nil
      case Const(a, as) => Const(a, init(as))
    }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Const(a, as) => f(a, foldRight(as, z)(f))
    }

    /**
      * Exercise 3.9
      * Returns the length of a list using foldRight.
      * Not stack-safe!
      */
    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, b) => 1 + b)
    }

    /**
      * Exercise 3.10
      * Tail-recursive implementation of foldRight.
      */
    @annotation.tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Const(h, t) => foldLeft(t, f(z, h))(f)
    }

    /**
      * Exercise 3.11
      * Sum, product, and length using foldLeft.
      */
    def sum(ds: List[Int]): Int = foldLeft(ds, 0)(_ + _)

    def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def length_2[A](as: List[A]): Int = {
      foldLeft(as, 0)((a, _) => 1 + a)
    }


  }

  def main(args: Array[String]): Unit = {

    val test = List(1, 2, 3, 4, 5, 6)

    println("Drop test: " + List.drop(test, 2))
    assert(List.drop(test, 2) == List(3, 4, 5, 6))

    println("DropWhile test: " + List.dropWhile(test, (x: Int) => x < 2))
    assert(List.dropWhile(test, (x: Int) => x < 2) == List(2, 3, 4, 5, 6))

    println("Init test: " + List.init(test))
    assert(List.init(test) == List(1, 2, 3, 4, 5))

    println("BetterDropWhile test: " + List.betterDropWhile(test)(x => x < 2))
    assert(List.betterDropWhile(test)(x => x < 2) == List(2, 3, 4, 5, 6))

    /**
      * Exercise 3.8
      */
    println("FoldRight with Nil and Const: " + List.foldRight(List(1, 2, 3), Nil: List[Int])(Const(_, _)))

    println("Length: " + List.length(List(1, 2, 3, 4)))

  }

}
