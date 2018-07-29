package training

import scala.collection.mutable.ListBuffer

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
            foldLeft( (2,3), 0+1 )(_ + _) // sum is evaluated eagerly, so 1 is passed to the next recursive call
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

    /**
      * Exercise 3.14
      * Append via fold.
      */
    def appendByFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, z) => Const(a, z))

    def appendByFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((z, a) => Const(a, z))

    /**
      * Exercise 3.15
      * Concatenate a list of lists into a single list.
      */
    def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(appendByFoldRight)

    /**
      * Exercise 3.16
      * Add 1 to each element.
      */
    def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, z) => Const(a + 1, z))

    /**
      * Exercise 3.17
      * Turn each double element into a string.
      */
    def toStrings(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, z) => Const(a.toString, z))

    /**
      * Exercise 3.18
      * Generic map function.
      */
    def map[A, B](l: List[A])(f: A => B): List[B] = foldRightByFoldLeft(l, Nil: List[B])((h, t) => Const(f(h), t))

    def map_1[A, B](l: List[A])(f: A => B): List[B] = {
      val result = new ListBuffer[B]

      def loop(l: List[A]): Unit = l match {
        case Nil => ()
        case Const(a, as) => result += f(a); loop(as)
      }

      loop(l)
      List(result.toList: _*)
    }

    /**
      * Exercise 3.19
      * Filter function.
      */
    def filter[A](l: List[A])(f: A => Boolean): List[A] = {
      val result = new ListBuffer[A]

      def loop(l: List[A]): Unit = l match {
        case Nil => ()
        case Const(a, as) =>
          if (f(a))
            result += a
          loop(as)
      }

      loop(l)
      List(result.toList: _*)
    }

    /**
      * Exercise 3.20
      * flatMap function.
      */
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      foldRightByFoldLeft(l, Nil: List[B])((h, t) => appendByFoldRight(f(h), t))

    /**
      * Exercise 3.21
      * filter via flatMap.
      */
    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
      flatMap(l)(a => if (f(a)) List(a) else Nil)
    }

    /**
      * Exercise 3.22
      * Add two lists.
      */
    def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (_, Nil) => l1
      case (Nil, _) => l2
      case (Const(a, as), Const(b, bs)) => Const(a + b, addTwoLists(as, bs))
    }

    /**
      * Exercise 3.23
      * Generalize the above function.
      */
    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Const(a, as), Const(b, bs)) => Const(f(a, b), zipWith(as, bs)(f))
    }

    /**
      * Exercise 3.24
      * Checks whether a list contains another list as a sub-sequence.
      */
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Const(a, as), Const(b, bs)) if a == b => startsWith(as, bs)
      case _ => false
    }

    @annotation.tailrec
    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
      case Nil => sub == Nil
      case _ if startsWith(l, sub) => true
      case Const(_, t) => hasSubsequence(t, sub)

    }

  }

  /**
    * ******************************************
    * ***************** Trees ******************
    * ******************************************
    */
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    /**
      * Exercise 3.25
      * Returns number of nodes in tree.
      */
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }


    /**
      * Exercise 3.26
      * Returns maximum element in Int tree.
      */
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }


    /**
      * Exercise 3.27
      * Returns maximum depth of a tree.
      */
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    /**
      * Exercise 3.28
      * Map function.
      */
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    /**
      * Exercise 3.29
      * Generic fold function.
      */
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => 1 + a + b)

    def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((a, b) => 1 + (a max b))

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
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

    println("Append by foldRight: " + List.appendByFoldRight(List(1, 2, 3, 4), List(5, 6)))
    println("Append by foldLeft: " + List.appendByFoldLeft(List(1, 2, 3, 4), List(5, 6)))

    println("Concat: " + List.concat(List(List(1, 2, 3, 4), List(5, 6), List(7))))

    println("Sum 1: " + List.add1(List(1, 2, 3)))

    println("ToStrings: " + List.toStrings(List(1, 2, 3)))

    println("Map: " + List.map(List(1, 2, 3))(a => a * a))
    println("Map_1: " + List.map_1(List(1, 2, 3))(a => a * a))

    println("Filter: " + List.filter(List(1, 2, 3))(a => a % 2 == 0))

    println("FlatMap: " + List.flatMap(List(1, 2, 3))(i => List(i, i)))

    println("filterViaFlatMap: " + List.filterViaFlatMap(List(1, 2, 3))(a => a % 2 == 0))

    println("addTwoLists: " + List.addTwoLists(List(1, 2, 3), List(4, 5, 6)))
    println("addTwoLists 2: " + List.addTwoLists(List(1, 2, 3), List(4)))

    println("zipWith: " + List.zipWith(List(1, 2, 3), List(4, 5))((a, b) => a * b))

    println("startsWith: " + List.startsWith(List(1, 2, 3), List(1, 2)))
    println("hasSubsequence: " + List.hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4)))

    /*
     Trees
      */
    println("Size: " + Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println("Max: " + Tree.maximum(Branch(Branch(Leaf(6), Leaf(2)), Leaf(3))))
    println("Depth: " + Tree.depth(
      Branch(
        Branch(
          Leaf(6),
          Branch(
            Leaf(6),
            Leaf(2))
        ),
        Leaf(3)))
    )
    println("Map: " + Tree.map(
      Branch(
        Branch(
          Leaf(6),
          Branch(
            Leaf(6),
            Leaf(2))
        ),
        Leaf(3))
    )(i => i * i))

    println("Size via fold: " + Tree.sizeViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println("Max via fold: " + Tree.maximumViaFold(Branch(Branch(Leaf(6), Leaf(2)), Leaf(3))))
    println("Depth via fold: " + Tree.depthViaFold(
      Branch(
        Branch(
          Leaf(6),
          Branch(
            Leaf(6),
            Leaf(2))
        ),
        Leaf(3)))
    )
    println("Map via fold: " + Tree.mapViaFold(
      Branch(
        Branch(
          Leaf(6),
          Branch(
            Leaf(6),
            Leaf(2))
        ),
        Leaf(3))
    )(i => i * i))
  }
}