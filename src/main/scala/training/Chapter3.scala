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

      /*
        Example for List(1,2,3) and _ + _ :

          foldRight( (1,2,3), 0 )(_ + _)
            1 + foldRight( (2,3), 0 )(_ + _)
              1 + (2 + foldRight( (3), 0 )(_ + _))
                1 + (2 + (3 + foldRight( Nil, 0 )(_ + _)))
                1 + (2 + (3 + 0))
              1 + (2 + 3)
            1 + 5
          6

       */
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

      /*
        Example for List(1,2,3), z=0 and _ + _ :

          foldLeft( (1,2,3), 0 )(_ + _)
            foldLeft( (2,3), 0+1 )(_ + _) // evaluated eagerly, so 1 is passed to the next recursive call
              foldLeft( (3), (0+1) + 2 )(_ + _)
                foldLeft( Nil, ((0+1) + 2) + 3)(_ + _)
                6
              6
            6
          6
       */
    }

    /**
      * Exercise 3.11
      * Sum, product, and length using foldLeft.
      */
    def sum(ds: List[Int]): Int = foldLeft(ds, 0)(_ + _)

    def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def lengthWithFoldLeft[A](as: List[A]): Int = {
      foldLeft(as, 0)((a, _) => 1 + a)
    }

    /**
      * Exercise 3.12
      * Return reverse of list.
      */
    def reverse[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Const(a, as) => foldLeft(as, List(a))((t, h) => Const(h, t))
    }

    /**
      * Exercise 3.13
      * Write foldRight in terms of foldLeft. [Hard]
      */
    def foldRightByFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(l, (b: B) => b)((g: B => B, a: A) => b => g(f(a, b)))(z)
      /*
      Example for List(1,2,3), z=0 and _ + _ :

        foldRightByFoldLeft( (1,2,3), 0 )(_ + _)
          foldLeft( (1,2,3), b => b )( (g,a) => b => g(a + b) )(0)
            foldLeft( (2,3), b => 1 + b )( (g,a) => b => g(a + b) )(0)
              foldLeft( (3), b => 1 + (2 + b) )( (g,a) => b => g(a + b) )(0)
                foldLeft( Nil, b => 1 + (2 + (3 + b)) )( (g,a) => b => g(a + b) )(0)
                (b => 1 + (2 + (3 + b)))(0)
              (b => 1 + (2 + (3 + b)))(0)
            (b => 1 + (2 + (3 + b)))(0)
          (b => 1 + (2 + (3 + b)))(0)
        6
     */
    }

    def sumWithFoldRightByFoldLeft(ds: List[Int]): Int = foldRightByFoldLeft(ds, 0)(_ + _)

    def lengthWithFoldRightByFoldLeft[A](as: List[A]): Int = {
      foldRightByFoldLeft(as, 0)((_, acc) => 1 + acc)
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

    println("Reverse: " + List.reverse(List(1, 2, 3)))

    println("Sum with stack-safe foldRight: " + List.foldRightByFoldLeft(List(1, 2, 3), 0)(_ + _))
    println("Length with stack-safe foldRight: " + List.lengthWithFoldLeft(List(1, 2, 3, 4)))

  }

}
